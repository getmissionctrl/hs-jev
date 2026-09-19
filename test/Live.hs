module Main (main) where

import Control.Monad (unless)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)

import Jev

main :: IO ()
main = do
  mk <- lookupEnv "TYPESAFE_API_KEY"
  case mk of
    Nothing -> putStrLn "live: TYPESAFE_API_KEY unset — skipping" >> exitSuccess
    Just _  -> runLive

-- | Unwrap a 'Right' or abort the run with a labelled error (opt-in test).
orDie :: Show e => String -> Either e a -> IO a
orDie msg = either (\e -> error (msg <> ": " <> show e)) pure

runLive :: IO ()
runLive = do
  client <- newClient
  let st = state
        ("Customer: I was double-charged for order A-104 and I'm furious." :: String)
      teamQ = askChoice (plain "Which team should handle this?")
                [ ("billing",  descPlain "Charges, invoices, payments")
                , ("returns",  descPlain "Refunds, wrong or damaged items")
                , ("shipping", descPlain "Delivery status, delays, lost packages") ]
      ask = (,) <$> teamQ <*> askNoul (plain "Should this be escalated to a human agent?")

  ((ca, escalate), usg) <- orDie "live call failed" =<< callJev client st ask
  putStrLn ("chosen team : " <> show ca.choice <> "  (confidence " <> show ca.confidence <> ")")
  putStrLn ("escalate?   : " <> show escalate)
  putStrLn ("usage       : " <> show usg)

  -- §5.2 independence spot-check: the same question asked alone must agree.
  (caAlone, _) <- orDie "live solo call failed" =<< callJev client st teamQ
  unless (caAlone.choice == ca.choice)
    (putStrLn ("WARNING: independence mismatch: "
               <> show caAlone.choice <> " vs " <> show ca.choice))
