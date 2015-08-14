{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.Timeout
import Control.Monad
import Data.Aeson hiding (Value)
import Data.Monoid
import Data.Time
import Data.Time.ISO8601
import Network.HTTP.Types hiding (Status)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Random
import Text.Printf
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

type ProposalId = Integer
type AcceptorId = T.Text
type Value      = T.Text

data PaxosMessage
  = Prepare       ProposalId
  | FreePromised  ProposalId AcceptorId
  | BoundPromised ProposalId AcceptorId ProposalId Value
  | Proposed                            ProposalId Value
  | Accepted                 AcceptorId ProposalId Value

data MessageType = PrepareType | PromisedType | ProposedType | AcceptedType

instance ToJSON MessageType where
  toJSON = String . \case
    PrepareType  -> "prepare"
    PromisedType -> "promised"
    ProposedType -> "proposed"
    AcceptedType -> "accepted"

instance FromJSON MessageType where
  parseJSON = withText "MessageType" $ \typeName -> case M.lookup typeName typesByName of
    Nothing -> mzero
    Just messageType -> return messageType
    where
    typesByName = M.fromList [("prepare", PrepareType), ("promised", PromisedType),
                             ("proposed", ProposedType), ("accepted", AcceptedType)]

instance ToJSON PaxosMessage where
  toJSON (Prepare proposalId) = object
    [ "type"     .= PrepareType
    , "proposal" .= proposalId
    ]

  toJSON (FreePromised proposalId acceptorId) = object
    [ "type"     .= PromisedType
    , "proposal" .= proposalId
    , "by"       .= acceptorId
    ]

  toJSON (BoundPromised proposalId acceptorId proposalId' value') = object
    [ "type"                  .= PromisedType
    , "proposal"              .= proposalId
    , "by"                    .= acceptorId
    , "max-accepted-proposal" .= proposalId'
    , "max-accepted-value"    .= value'
    ]

  toJSON (Proposed proposalId value) = object
    [ "type"     .= ProposedType
    , "proposal" .= proposalId
    , "value"    .= value
    ]

  toJSON (Accepted acceptorId proposalId value) = object
    [ "type"     .= AcceptedType
    , "proposal" .= proposalId
    , "by"       .= acceptorId
    , "value"    .= value
    ]

instance FromJSON PaxosMessage where
  parseJSON = withObject "PaxosMessage" $ \o -> do
    let messageType = o .: "type"
        proposalId  = o .: "proposal"
        acceptorId  = o .: "by"
        value       = o .: "value"
    messageType >>= \case
      PrepareType  -> Prepare  <$> proposalId
      ProposedType -> Proposed <$> proposalId <*> value
      AcceptedType -> Accepted <$> acceptorId <*> proposalId <*> value
      PromisedType
        ->  (BoundPromised <$> proposalId <*> acceptorId
                <*> o .: "max-accepted-proposal"
                <*> o .: "max-accepted-value")
        <|> (FreePromised <$> proposalId <*> acceptorId)

data Config = Config
  { cGetTimeout     :: Integer
  , cQueueExpiry    :: Integer
  , cProposerCount  :: Integer
  , cDropPercentage :: Double
  , cMinDelay       :: Int
  , cMaxDelay       :: Int
  }

instance ToJSON Config where
  toJSON Config{..} = object
    [ "get-timeout-us"  .= cGetTimeout
    , "queue-expiry-us" .= cQueueExpiry
    , "proposer-count"  .= cProposerCount
    , "drop-percentage" .= cDropPercentage
    , "min-delay-us"    .= cMinDelay
    , "max-delay-us"    .= cMaxDelay
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config
    <$> o .: "get-timeout-us"
    <*> o .: "queue-expiry-us"
    <*> o .: "proposer-count"
    <*> o .: "drop-percentage"
    <*> o .: "min-delay-us"
    <*> o .: "max-delay-us"

data Status = Status Config [(B.ByteString, UTCTime, Bool)]

instance ToJSON Status where
  toJSON (Status config queues) = object
    [ "config" .= config
    , "queues" .= map (\(queueName, lastGetTime, isEmpty) -> object
        [ "queue" .= T.decodeUtf8 queueName
        , "last-get-time" .= formatISO8601Micros lastGetTime
        , "is-empty" .= isEmpty
        ]) queues
    ]

main :: IO ()
main = do
  outgoingQueueByNameVar <- newTVarIO M.empty
  incomingQueue          <- newTQueueIO
  logLock                <- newMVar ()
  configVar              <- newTVarIO $ Config
    { cGetTimeout     = 10000000
    , cQueueExpiry    = 60000000
    , cProposerCount  = 10
    , cDropPercentage = 0
    , cMinDelay       = 0
    , cMaxDelay       = 0
    }

  let logMessage :: UTCTime -> B.ByteString -> String -> IO ()
      logMessage time queueName message = void $ forkIO $ withMVar logLock $ \_ -> putStrLn
        $ printf "%-27s %-15s %s" (formatISO8601Micros time) (T.unpack $ T.decodeUtf8 queueName) message

  withAsync (run 24192 $ \req respond -> do
    now <- getCurrentTime
    let queueName = rawPathInfo req

        corsHeaders =
          [("Access-Control-Allow-Origin", T.encodeUtf8 "*")
          ,("Access-Control-Allow-Headers", T.encodeUtf8 "Content-Type")
          ]

        respondJson :: ToJSON a => a -> IO ResponseReceived
        respondJson = respond . responseLBS ok200
          ((hContentType, T.encodeUtf8 "application/json") : corsHeaders) . encode

        respondEmpty      = respond $ responseLBS noContent204  corsHeaders mempty
        respondBadRequest = respond $ responseLBS badRequest400 corsHeaders mempty

    case parseMethod $ requestMethod req of
      Right GET
        | queueName == "/status" -> do

            status <- atomically $ do
              outgoingQueueByName <- readTVar outgoingQueueByNameVar
              Status <$> readTVar configVar
                     <*> forM (M.toList outgoingQueueByName)
                  (\(queueName, (lastAccessTime, queue)) ->
                    (,,) queueName lastAccessTime <$> isEmptyTQueue queue)

            respondJson status

        | queueName == "/config" -> do

            config <- atomically $ readTVar configVar
            respondJson config

        | otherwise -> do

            (theQueue, Config{..}) <- atomically $ do
              outgoingQueueByName <- readTVar outgoingQueueByNameVar
              theQueue <- case M.lookup queueName outgoingQueueByName of
                Nothing                 -> newTQueue
                Just (_, existingQueue) -> return existingQueue
              writeTVar outgoingQueueByNameVar $ M.insert queueName (now, theQueue) outgoingQueueByName
              (,) theQueue <$> readTVar configVar

            response <- timeout cGetTimeout $ atomically $ readTQueue theQueue

            case response of
              Nothing -> respondEmpty
              Just value -> do
                let valueBytes = encode (value :: PaxosMessage)
                responseTime <- getCurrentTime
                logMessage responseTime queueName $ "GET  " ++ (T.unpack $ T.decodeUtf8 $ BL.toStrict valueBytes)
                respondJson value

      Right POST
        | queueName == "/config" -> do
            body <- strictRequestBody req
            let maybeValue = decode body
            case maybeValue of
              Nothing -> respondBadRequest
              Just value -> do
                atomically $ writeTVar configVar value
                respondEmpty

        | otherwise -> do
            body <- strictRequestBody req
            let maybeValue = decode body
            case maybeValue of
              Nothing -> respondBadRequest
              Just value -> do
                logMessage now queueName $ "POST " ++ (T.unpack $ T.decodeUtf8 $ BL.toStrict $ encode value)
                atomically $ writeTQueue incomingQueue (queueName, now, value :: PaxosMessage)
                respondEmpty

      _ -> respondEmpty)



    $ \_ -> forever $ join $ atomically $ do

        (queueName, receivedTime, value) <- readTQueue incomingQueue
        Config{..} <- readTVar configVar

        let staleIfNotQueriedSince = addUTCTime (fromIntegral cQueueExpiry * negate 0.000001) receivedTime
        (activeQueuesMap, staleQueuesMap) <- M.partition ((> staleIfNotQueriedSince) . fst)
            <$> readTVar outgoingQueueByNameVar
        writeTVar outgoingQueueByNameVar activeQueuesMap

        let isRelevantProposer proposalId = (== (T.encodeUtf8 ("/proposer/" <> T.pack (show (mod proposalId cProposerCount)))))

            shouldOutputTo = case value of
              Prepare  {} -> B.isPrefixOf (T.encodeUtf8 "/acceptor/")
              Proposed {} -> B.isPrefixOf (T.encodeUtf8 "/acceptor/")
              Accepted {} -> B.isPrefixOf (T.encodeUtf8 "/learner/")
              FreePromised  proposalId _     -> isRelevantProposer proposalId
              BoundPromised proposalId _ _ _ -> isRelevantProposer proposalId

            outputQueues = [ queue
                           | (queueName, (_, queue)) <- M.toList activeQueuesMap
                           , shouldOutputTo queueName ]

        return $ do
          forM_ (M.keys staleQueuesMap) $ \staleQueueName ->
            logMessage receivedTime staleQueueName ("expired at " <> formatISO8601Millis staleIfNotQueriedSince)
          forM_ outputQueues $ \queue -> do
            dropRV  <- randomRIO (0.0, 100.0)
            delayRV <- randomRIO (cMinDelay, cMaxDelay)
            when (dropRV >= cDropPercentage) $ void $ forkIO $ do
              threadDelay delayRV
              atomically $ writeTQueue queue value

