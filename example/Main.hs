-- | Recreated TypeSafe Jev documentation examples, as one executable.
--
-- Each example issues a real @callJev@ against the live API, so a
-- @TYPESAFE_API_KEY@ must be set in the environment. Pick one by name:
--
-- > cabal run hs-jev-examples -- --list
-- > cabal run hs-jev-examples -- support-triage
module Main (main) where

import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.List (find)
import Data.Text (Text)
import Options.Applicative
import System.Exit (exitFailure)
import Text.Printf (printf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Jev

-- Registry --------------------------------------------------------------------

data Example = Example
  { exName :: String
  , exDesc :: String
  , exRun  :: Client -> IO ()
  }

examples :: [Example]
examples =
  [ Example "support-triage"
      "Route a support ticket — Noul + Choice + Score in one batched call"
      supportTriage
  , Example "spam-check"
      "Speculative fan-out — six spam-signal Nouls over one email"
      spamCheck
  , Example "resume-screen"
      "Composite scoring — four Score dimensions weighted per role"
      resumeScreen
  ]

-- CLI -------------------------------------------------------------------------

data Cmd = List | Run String

cmd :: Parser Cmd
cmd =
      flag' List (long "list" <> short 'l' <> help "List available examples and exit")
  <|> (Run <$> strArgument (metavar "EXAMPLE" <> help "Example to run (see --list)"))

main :: IO ()
main = execParser opts >>= dispatch
  where
    opts = info (cmd <**> helper)
      ( fullDesc
     <> header "hs-jev-examples — recreated TypeSafe Jev documentation examples"
     <> progDesc "Run a Jev doc example against the live API (needs TYPESAFE_API_KEY)." )

dispatch :: Cmd -> IO ()
dispatch List = do
  putStrLn "Available examples:"
  forM_ examples $ \e ->
    putStrLn ("  " <> padTo 16 (exName e) <> exDesc e)
dispatch (Run name) =
  case find ((== name) . exName) examples of
    Nothing -> do
      putStrLn ("Unknown example: " <> name)
      putStrLn "Use --list to see available examples."
      exitFailure
    Just e -> do
      client <- newClient
      putStrLn ("=== " <> exName e <> " ===")
      exRun e client

-- Examples --------------------------------------------------------------------

-- | Support-ticket triage (quickstart): one Noul, one Choice, one Score.
supportTriage :: Client -> IO ()
supportTriage client = do
  let st = state $ object
        [ "ticket" .= object
            [ "subject"  .= ("Duplicate charge" :: Text)
            , "messages" .=
                [ object [ "from" .= ("customer" :: Text)
                         , "text" .= ("I was charged twice for order A-104 — please refund the extra charge." :: Text) ] ] ]
        , "order" .= object
            [ "id"      .= ("A-104" :: Text)
            , "charges" .= [ object [ "amount_usd" .= (49 :: Int), "status" .= ("captured" :: Text) ]
                           , object [ "amount_usd" .= (49 :: Int), "status" .= ("captured" :: Text) ] ] ]
        , "refund_policy" .= ("Duplicate charges are eligible for a refund." :: Text) ]
      ask = (,,)
        <$> askNoul (plain "Does the customer's message request a refund?")
        <*> askChoice (plain "Which team should handle this ticket?")
              [ ("returns",  descPlain "Exchanges, refunds, wrong or damaged items")
              , ("shipping", descPlain "Delivery status, delays, lost packages")
              , ("billing",  descPlain "Charges, invoices, payment problems") ]
        <*> askScore (plain "How severe is the reported issue?")
              [ plain "Cosmetic; no impact to functionality"
              , plain "Broken or degraded feature, but workaround exists"
              , plain "Blocking issue; no workaround exists" ]
  ((refund, dept, severity), usg) <- callOrDie client st ask
  putStrLn ("refund requested : " <> f2 refund)
  putChoice "department" dept
  putScore  "severity" severity
  putUsage usg

-- | Spam detection by speculative fan-out: six independent Noul signals,
-- combined in code with a three-band verdict on the strongest signal.
spamCheck :: Client -> IO ()
spamCheck client = do
  let st = state $ object
        [ "from"    .= ("security@paypa1-support.com" :: Text)
        , "subject" .= ("Urgent: verify your account within 24 hours" :: Text)
        , "body"    .= ("Your account is locked. Confirm your password at http://bit.ly/xy2 to avoid suspension and claim your $100 account credit." :: Text) ]
      signals :: [Text]
      signals =
        [ "The message asks the recipient for account credentials or a password."
        , "The sender address is inconsistent with the organisation it claims to be from."
        , "The message offers an unexpected reward, prize, or credit."
        , "The message creates urgency or time pressure to act immediately."
        , "The visible link text does not match its actual destination."
        , "A link is shortened or obfuscated to hide its true destination." ]
      ask = traverse (askNoul . plain) signals
  (probs, usg) <- callOrDie client st ask
  forM_ (zip signals probs) $ \(sig, p) ->
    putStrLn ("  [" <> f2 p <> "] " <> T.unpack sig)
  let worst = maximum probs
  putStrLn ("overall spam signal (max): " <> f2 worst <> "  -> " <> show (band (0.34, 0.67) worst))
  putUsage usg

-- | Composite scoring: four Score dimensions, weighted differently per role.
resumeScreen :: Client -> IO ()
resumeScreen client = do
  let st = state ("Staff engineer, 9 yrs. Led a 6-person platform team for 3 years; designed a multi-region event pipeline handling 2B events/day. Primary language Python; also shipped Rust and TypeScript services. Moved from ML tooling into infra and later developer experience." :: Text)
      lv = map plain
      ask = (,,,)
        <$> askScore (plain "How much depth of Python experience does this candidate have, based on the supplied resume?")
              (lv ["No experience","Mentioned only","Used in projects","Primary language","Deep expertise"])
        <*> askScore (plain "How much experience does this candidate have managing or leading engineering teams?")
              (lv ["No management","Informal mentorship","Led small team","Managed direct reports","Managed multiple teams"])
        <*> askScore (plain "How much experience does this candidate have designing large-scale or distributed systems?")
              (lv ["No architecture","Contributed to discussions","Designed components","Owned significant system","Designed at scale across domains"])
        <*> askScore (plain "How much evidence is there that this candidate picks up unfamiliar tools, roles, or domains outside their core specialty?")
              (lv ["One domain only","Narrow variety","Few different areas","Regular domain switching","Track record ramping up unfamiliar areas"])
  ((py, lead, arch, gen), usg) <- callOrDie client st ask
  putScore "python_depth" py
  putScore "team_leadership" lead
  putScore "system_design" arch
  putScore "generalist" gen
  let seniorIC = compositeScore [(0.40, py), (0.10, lead), (0.40, arch), (0.10, gen)]
      engMgr   = compositeScore [(0.15, py), (0.40, lead), (0.20, arch), (0.25, gen)]
  putStrLn ("composite (Senior IC weighting)          : " <> f2 seniorIC)
  putStrLn ("composite (Engineering Manager weighting) : " <> f2 engMgr)
  putUsage usg

-- Helpers ---------------------------------------------------------------------

callOrDie :: Client -> State -> Ask a -> IO (a, Usage)
callOrDie c s a = callJev c s a >>= either (\e -> error ("Jev call failed: " <> show e)) pure

putChoice :: String -> ChoiceAnswer -> IO ()
putChoice label ca = do
  putStrLn (label <> " : " <> T.unpack ca.choice <> "  (confidence " <> f2 ca.confidence <> ")")
  forM_ (Map.toList ca.probabilities) $ \(k, p) ->
    putStrLn ("    " <> padTo 10 (T.unpack k) <> f2 p)

putScore :: String -> ScoreAnswer -> IO ()
putScore label sa =
  putStrLn (label <> " : score " <> f2 sa.score <> " / " <> show (Map.size sa.probabilities - 1)
            <> "  (confidence " <> f2 sa.confidence <> ")")

putUsage :: Usage -> IO ()
putUsage u = putStrLn ("  usage: in=" <> show u.inputTokens <> " out=" <> show u.outputTokens)

f2 :: Double -> String
f2 = printf "%.2f"

padTo :: Int -> String -> String
padTo n s = s <> replicate (max 0 (n - length s)) ' '
