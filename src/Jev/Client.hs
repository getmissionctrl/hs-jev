module Jev.Client
  ( RetryPolicy(..), defaultRetry, shouldRetry
  , Config(..), defaultConfig
  , Client(..), newClient, newClientWith
  , callJev
  ) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry (RetryStatus(..), limitRetries, exponentialBackoff, retrying)
import Data.Aeson (Value, encode, object, (.=), eitherDecode)
import Data.Text (Text)
import System.Environment (lookupEnv)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson.KeyMap as KM
import Network.HTTP.Client hiding (Response)
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Network.HTTP.Types.Status as HTS

import Jev.Types
import Jev.Ask (Ask, encodeQuestions, decodeAnswers)

data RetryPolicy = RetryPolicy { maxRetries :: Int, baseDelayMs :: Int }

defaultRetry :: RetryPolicy
defaultRetry = RetryPolicy 3 200

-- | Retry on rate-limit (429) and server (5xx) statuses.
shouldRetry :: Int -> Bool
shouldRetry s = s == 429 || (s >= 500 && s < 600)

data Config = Config
  { apiKey        :: Text
  , baseUrl       :: Text
  , model         :: Text
  , timeoutMicros :: Int
  , retryPolicy   :: RetryPolicy
  , onEvent       :: JevEvent -> IO ()
  }

defaultConfig :: Text -> Config
defaultConfig k = Config
  { apiKey = k
  , baseUrl = "https://api.typesafe.ai"
  , model = "jev-latest"
  , timeoutMicros = 60 * 1000000
  , retryPolicy = defaultRetry
  , onEvent = const (pure ())
  }

data Client = Client { manager :: Manager, config :: Config }

-- | Build a client from @TYPESAFE_API_KEY@ with default config.
newClient :: MonadIO m => m Client
newClient = liftIO $ do
  mk <- lookupEnv "TYPESAFE_API_KEY"
  case mk of
    Nothing -> ioError (userError "TYPESAFE_API_KEY not set")
    Just k  -> newClientWith (defaultConfig (T.pack k))

newClientWith :: MonadIO m => Config -> m Client
newClientWith cfg = liftIO $ do
  mgr <- newTlsManager
  pure (Client mgr cfg)

-- | Send one batch of questions over the shared state; return the typed result
-- and the reported token usage, or a 'JevError'.
callJev :: MonadIO m => Client -> State -> Ask a -> m (Either JevError (a, Usage))
callJev (Client mgr cfg) st ask = liftIO $ do
  let qs   = encodeQuestions ask
      body = object [ "model" .= model cfg, "state" .= st, "questions" .= qs ]
  onEvent cfg (Requested (KM.size qs) (fromIntegral (BL.length (encode st))))
  let policy = exponentialBackoff (baseDelayMs (retryPolicy cfg) * 1000)
               <> limitRetries (maxRetries (retryPolicy cfg))
  final <- retrying policy (\rs r -> onRetry rs r >> pure (retryable r)) (\_ -> attemptOnce body)
  let result = do resp <- final
                  a    <- decodeAnswers ask resp.answers
                  pure (a, resp.usage)
  case result of
    Left err     -> onEvent cfg (Failed err)    >> pure (Left err)
    Right (a, u) -> onEvent cfg (Responded u 0) >> pure (Right (a, u))
  where
    retryable (Left (ApiError s _))     = shouldRetry s
    retryable (Left (TransportError _)) = True
    retryable _                         = False

    onRetry (RetryStatus n _ _) (Left (ApiError s _)) = onEvent cfg (Retried n s)
    onRetry _ _                                       = pure ()

    attemptOnce :: Value -> IO (Either JevError Jev.Types.Response)
    attemptOnce body = do
      req0 <- parseRequest (T.unpack (baseUrl cfg) <> "/v1/systemone")
      let req = req0
            { method = "POST"
            , requestHeaders =
                [ ("Authorization", "Bearer " <> TE.encodeUtf8 (apiKey cfg))
                , ("Content-Type", "application/json") ]
            , requestBody = RequestBodyLBS (encode body)
            , responseTimeout = responseTimeoutMicro (timeoutMicros cfg)
            }
      e <- try (httpLbs req mgr)
      pure $ case e of
        Left ex -> Left (TransportError ex)
        Right r ->
          let code = HTS.statusCode (responseStatus r)
              bs   = responseBody r
          in if code >= 200 && code < 300
               then case eitherDecode bs of
                      Left de   -> Left (DecodeError de (jsonOrNull bs))
                      Right rsp -> Right rsp
               else Left (ApiError code (TE.decodeUtf8 (BL.toStrict bs)))

    jsonOrNull bs = maybe (object []) id (eitherToMaybe (eitherDecode bs))
    eitherToMaybe = either (const Nothing) Just
