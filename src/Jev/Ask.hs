module Jev.Ask
  ( Ask, QF(..)
  , askChoice, askNoul, askScore
  , ChoiceResult(..), askChoiceEnum, toEnumResult
  , encodeQuestions, decodeAnswers
  ) where

import Control.Applicative.Free (Ap, liftAp, runAp, runAp_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Jev.Types

-- | One question: its rendered JSON payload, plus how to parse its answer.
data QF a = QF
  { qJson  :: Value
  , qParse :: Value -> Either JevError a
  }

-- | A batch of typed questions. Free applicative: '<*>' batches into one call.
type Ask = Ap QF

askChoice :: Instructions -> Criteria -> Ask ChoiceAnswer
askChoice instr crit = liftAp (QF (choiceJson instr crit) decodeVal)

askNoul :: Instructions -> Ask Double
askNoul instr = liftAp (QF (noulJson instr) parseNoul)

askScore :: Instructions -> [Level] -> Ask ScoreAnswer
askScore instr lvls = liftAp (QF (scoreJson instr lvls) decodeVal)

-- Question JSON --------------------------------------------------------------

choiceJson :: Instructions -> Criteria -> Value
choiceJson instr crit =
  object [ "type" .= ("choice" :: Text), "instructions" .= instr
         , "criteria" .= criteriaJson crit ]

noulJson :: Instructions -> Value
noulJson instr = object [ "type" .= ("noul" :: Text), "instructions" .= instr ]

scoreJson :: Instructions -> [Level] -> Value
scoreJson instr lvls =
  object [ "type" .= ("score" :: Text), "instructions" .= instr, "criteria" .= lvls ]

criteriaJson :: Criteria -> Value
criteriaJson crit =
  object [ Key.fromText k .= fromMaybe Null md | (k, Description md) <- crit ]

-- Interpreter 1: what we send ------------------------------------------------

-- | Render the batch to a @questions@ map under positional keys @q0..qn@.
encodeQuestions :: Ask a -> KM.KeyMap Value
encodeQuestions =
  KM.fromList
    . zipWith (\i j -> (Key.fromString ('q' : show (i :: Int)), j)) [0 ..]
    . runAp_ (\q -> [qJson q])

-- Interpreter 2: reconstruct the typed result --------------------------------

-- | Decode the @answers@ map back into the typed result. Positional keys match
-- 'encodeQuestions' because both traverse the 'Ap' spine left-to-right.
decodeAnswers :: Ask a -> KM.KeyMap Value -> Either JevError a
decodeAnswers ask ans = evalStateT (runAp step ask) 0
  where
    step :: QF x -> StateT Int (Either JevError) x
    step q = do
      i <- get
      put (i + 1)
      let k = Key.fromString ('q' : show i)
      case KM.lookup k ans of
        Nothing -> lift (Left (MissingAnswer (T.pack ('q' : show i))))
        Just v  -> lift (qParse q v)

-- Enum sugar -----------------------------------------------------------------

data ChoiceResult o = ChoiceResult
  { chosen     :: o
  , dist       :: Map o Double
  , confidence :: Double
  } deriving (Eq, Show)

-- | Choice over a closed Haskell enum: criteria are built from the enum's
-- 'show', and the answer is re-associated back to constructors.
askChoiceEnum :: forall o. (Bounded o, Enum o, Ord o, Show o)
              => Instructions -> (o -> Description) -> Ask (ChoiceResult o)
askChoiceEnum instr describe = liftAp (QF (choiceJson instr crit) parse)
  where
    os   = [minBound .. maxBound] :: [o]
    crit = [ (T.pack (show o), describe o) | o <- os ]
    rev  = Map.fromList [ (T.pack (show o), o) | o <- os ]
    parse v = decodeVal v >>= toEnumResult rev

-- | Re-associate a raw 'ChoiceAnswer' to an enum via a reverse (show -> o) map.
toEnumResult :: Ord o => Map Text o -> ChoiceAnswer -> Either JevError (ChoiceResult o)
toEnumResult rev ca = do
  ch <- lk ca.choice
  ps <- traverse (\(k, p) -> (\o -> (o, p)) <$> lk k) (Map.toList ca.probabilities)
  pure (ChoiceResult ch (Map.fromList ps) ca.confidence)
  where lk k = maybe (Left (UnknownChoiceKey k (Map.keys rev))) Right (Map.lookup k rev)
