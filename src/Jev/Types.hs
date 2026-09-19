module Jev.Types
  ( -- * Request-side
    State(..), state
  , Instructions(..), plain, structured
  , Description(..), descPlain, descNone
  , Criteria, Level
    -- * Answers
  , ChoiceAnswer(..), ScoreAnswer(..)
  , Usage(..), Response(..)
    -- * Errors & events
  , JevError(..), JevEvent(..)
    -- * Decoding helper
  , decodeVal, parseNoul
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Network.HTTP.Client (HttpException)

-- Request-side ---------------------------------------------------------------

newtype State = State Value
instance ToJSON State where toJSON (State v) = v

-- | Wrap any JSON-encodable value as the request state.
state :: ToJSON a => a -> State
state = State . toJSON

-- | Instructions: a string, or a structured object (what/not_for/examples).
newtype Instructions = Instructions Value
instance ToJSON Instructions where toJSON (Instructions v) = v

plain :: Text -> Instructions
plain = Instructions . String

structured :: [(Text, Value)] -> Instructions
structured = Instructions . object . map (\(k, v) -> Key.fromText k .= v)

-- | A Choice option description; 'Nothing' serialises to JSON null.
newtype Description = Description (Maybe Value)

descPlain :: Text -> Description
descPlain = Description . Just . String

descNone :: Description
descNone = Description Nothing

-- | Ordered Choice options (name, description); at most 255.
type Criteria = [(Text, Description)]

-- | An ordered Score level description (2..10 of them).
type Level = Instructions

-- Answers --------------------------------------------------------------------

data ChoiceAnswer = ChoiceAnswer
  { choice        :: Text
  , probabilities :: Map Text Double
  , confidence    :: Double
  } deriving (Eq, Show)

instance FromJSON ChoiceAnswer where
  parseJSON = withObject "ChoiceAnswer" $ \o ->
    ChoiceAnswer <$> o .: "choice" <*> o .: "probabilities" <*> o .: "confidence"

data ScoreAnswer = ScoreAnswer
  { score         :: Double
  , legend        :: Map Int Text
  , probabilities :: Map Int Double
  , confidence    :: Double
  } deriving (Eq, Show)

instance FromJSON ScoreAnswer where
  parseJSON = withObject "ScoreAnswer" $ \o ->
    ScoreAnswer <$> o .: "score" <*> o .: "legend" <*> o .: "probabilities" <*> o .: "confidence"

data Usage = Usage { inputTokens :: Int, outputTokens :: Int } deriving (Eq, Show)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \o ->
    Usage <$> o .: "input_tokens" <*> o .: "output_tokens"

data Response = Response { answers :: KM.KeyMap Value, usage :: Usage }

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o ->
    Response <$> o .: "answers" <*> o .: "usage"

-- Errors & events ------------------------------------------------------------

data JevError
  = TransportError HttpException
  | ApiError Int Text
  | DecodeError String Value
  | MissingAnswer Text
  | UnknownChoiceKey Text [Text]
  deriving (Show)

data JevEvent
  = Requested { questionCount :: Int, stateBytes :: Int }
  | Responded { usage :: Usage, latencyMs :: Int }
  | Retried   { attempt :: Int, statusCode :: Int }
  | Failed    JevError
  deriving (Show)

-- Decoding helpers -----------------------------------------------------------

-- | Decode a JSON value into any 'FromJSON' type, tagging failures.
decodeVal :: FromJSON a => Value -> Either JevError a
decodeVal v = first (\e -> DecodeError e v) (parseEither parseJSON v)

-- | Parse a Noul answer object @{"noul": p}@ to the bare probability.
parseNoul :: Value -> Either JevError Double
parseNoul v = first (\e -> DecodeError e v) (parseEither (withObject "noul" (\o -> o .: "noul")) v)
