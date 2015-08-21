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
import Data.Aeson.Types (Pair)
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

type InstanceId = Integer
type TimePeriod = Integer
type AcceptorId = T.Text
type Value      = T.Text

data PaxosMessage
  = Prepare       (Maybe InstanceId) TimePeriod
  | MultiPromised        InstanceId  TimePeriod AcceptorId
  | FreePromised  (Maybe InstanceId) TimePeriod AcceptorId
  | BoundPromised (Maybe InstanceId) TimePeriod AcceptorId TimePeriod Value
  | Proposed      (Maybe InstanceId)                       TimePeriod Value
  | Accepted      (Maybe InstanceId)            AcceptorId TimePeriod Value

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

instanceIfJust :: Maybe InstanceId -> [Pair]
instanceIfJust = maybe [] $ return . (.=) "instance"

instance ToJSON PaxosMessage where
  toJSON (Prepare maybeInstance timePeriod) = object $
    [ "type"       .= PrepareType
    , "timePeriod" .= timePeriod
    ] ++ maybe []
               (\i -> ["instance" .= i, "includesGreaterInstances" .= True])
               maybeInstance

  toJSON (MultiPromised instanceId timePeriod acceptorId) = object
    [ "type"       .= PromisedType
    , "timePeriod" .= timePeriod
    , "by"         .= acceptorId
    , "instance"   .= instanceId
    , "includesGreaterInstances" .= True
    ]

  toJSON (FreePromised maybeInstance timePeriod acceptorId) = object $
    [ "type"       .= PromisedType
    , "timePeriod" .= timePeriod
    , "by"         .= acceptorId
    ] ++ instanceIfJust maybeInstance

  toJSON (BoundPromised maybeInstance timePeriod acceptorId timePeriod' value') = object $
    [ "type"                   .= PromisedType
    , "timePeriod"             .= timePeriod
    , "by"                     .= acceptorId
    , "lastAcceptedTimePeriod" .= timePeriod'
    , "lastAcceptedValue"      .= value'
    ] ++ instanceIfJust maybeInstance

  toJSON (Proposed maybeInstance timePeriod value) = object $
    [ "type"       .= ProposedType
    , "timePeriod" .= timePeriod
    , "value"      .= value
    ] ++ instanceIfJust maybeInstance

  toJSON (Accepted maybeInstance acceptorId timePeriod value) = object $
    [ "type"       .= AcceptedType
    , "timePeriod" .= timePeriod
    , "by"         .= acceptorId
    , "value"      .= value
    ] ++ instanceIfJust maybeInstance

instance FromJSON PaxosMessage where
  parseJSON = withObject "PaxosMessage" $ \o -> do
    let messageType = o .: "type"
        timePeriod  = o .: "timePeriod"
        acceptorId  = o .: "by"
        value       = o .: "value"
        maybeInstance = o .:? "instance"
    messageType >>= \case
      PrepareType  -> Prepare  <$> maybeInstance <*> timePeriod
      ProposedType -> Proposed <$> maybeInstance <*> timePeriod <*> value
      AcceptedType -> Accepted <$> maybeInstance <*> acceptorId <*> timePeriod <*> value
      PromisedType
        ->  (BoundPromised
                <$> maybeInstance
                <*> timePeriod
                <*> acceptorId
                <*> o .: "lastAcceptedTimePeriod"
                <*> o .: "lastAcceptedValue")
        <|> (MultiPromised
                <$> o .: "instance"
                <*> timePeriod
                <*> acceptorId) -- TODO need to check the includesGreatestInstances field
        <|> (FreePromised <$> maybeInstance <*> timePeriod <*> acceptorId)
        -- TODO worth checking there are no other fields

data Config = Config
  { cGetTimeoutSec  :: Integer
  , cQueueExpirySec :: Integer
  , cProposerCount  :: Integer
  , cDropPercentage :: Double
  , cMinDelaySec    :: Int
  , cMaxDelaySec    :: Int
  , cNagPeriodSec   :: Int
  }

instance ToJSON Config where
  toJSON Config{..} = object
    [ "get-timeout-sec"  .= cGetTimeoutSec
    , "queue-expiry-sec" .= cQueueExpirySec
    , "proposer-count"   .= cProposerCount
    , "drop-percentage"  .= cDropPercentage
    , "min-delay-sec"    .= cMinDelaySec
    , "max-delay-sec"    .= cMaxDelaySec
    , "nag-period-sec"   .= cNagPeriodSec
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config
    <$> o .: "get-timeout-sec"
    <*> o .: "queue-expiry-sec"
    <*> o .: "proposer-count"
    <*> o .: "drop-percentage"
    <*> o .: "min-delay-sec"
    <*> o .: "max-delay-sec"
    <*> o .: "nag-period-sec"

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

unixEpoch :: UTCTime
unixEpoch = UTCTime (fromGregorian 1970 01 01) 0

main :: IO ()
main = do
  outgoingQueueByNameVar <- newTVarIO M.empty
  incomingQueue          <- newTQueueIO
  logLock                <- newMVar ()
  configVar              <- newTVarIO $ Config
    { cGetTimeoutSec  = 10
    , cQueueExpirySec = 60
    , cProposerCount  = 10
    , cDropPercentage = 0
    , cMinDelaySec    = 0
    , cMaxDelaySec    = 0
    , cNagPeriodSec   = 5
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
          ,("Cache-Control", T.encodeUtf8 "no-cache, no-store, must-revalidate")
          ,("Pragma", T.encodeUtf8 "no-cache")
          ,("Expires", T.encodeUtf8 "0")
          ]

        respondJson :: ToJSON a => a -> IO ResponseReceived
        respondJson = respond . responseLBS ok200
          ((hContentType, T.encodeUtf8 "application/json; charset=utf-8") : corsHeaders) . encode

        respondEmpty      = respond $ responseLBS noContent204        corsHeaders mempty
        respondBadRequest = respond $ responseLBS badRequest400       corsHeaders mempty
        respondBadMethod  = respond $ responseLBS methodNotAllowed405 corsHeaders mempty

    case parseMethod $ requestMethod req of
      Right OPTIONS -> respond $ responseLBS noContent204
          (("Allow", T.encodeUtf8 "GET,POST,OPTIONS"): corsHeaders) mempty

      Right GET
        | queueName == "/status" -> do

            status <- atomically $ do
              outgoingQueueByName <- readTVar outgoingQueueByNameVar
              Status <$> readTVar configVar
                     <*> forM (M.toList outgoingQueueByName)
                  (\(queueName', (lastAccessTime, queue)) ->
                    (,,) queueName' lastAccessTime <$> isEmptyTQueue queue)

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

            response <- timeout (cGetTimeoutSec * 1000000) $ atomically $ readTQueue theQueue

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

      _ -> respondBadMethod)



    $ \_ -> withAsync (forever $ do
        
        nagPeriodSec <- cNagPeriodSec <$> readTVarIO configVar
        threadDelay $ nagPeriodSec * 1000000

        now <- getCurrentTime
        let value = Prepare Nothing $ floor $ diffUTCTime now unixEpoch
        logMessage now "/nag" $ "POST " ++ (T.unpack $ T.decodeUtf8 $ BL.toStrict $ encode value)
        atomically $ writeTQueue incomingQueue ("/nag", now, value))

    $ \_ -> forever $ join $ atomically $ do

        (incomingQueue, receivedTime, value) <- readTQueue incomingQueue
        Config{..} <- readTVar configVar

        let staleIfNotQueriedSince = addUTCTime (fromIntegral $ negate cQueueExpirySec) receivedTime
        (activeQueuesMap, staleQueuesMap) <- M.partition ((> staleIfNotQueriedSince) . fst)
            <$> readTVar outgoingQueueByNameVar
        writeTVar outgoingQueueByNameVar activeQueuesMap

        let isRelevantProposer timePeriod
              = (`elem` [ T.encodeUtf8 (nodeType <> T.pack (show (mod timePeriod cProposerCount)))
                        | nodeType <- ["/proposer/", "/general/"]])

            prefixIsOneOf prefixes qn = or [ B.isPrefixOf (T.encodeUtf8 prefix) qn | prefix <- prefixes ]
            isAcceptor = prefixIsOneOf ["/acceptor/", "/general/"]
            isLearner  = prefixIsOneOf ["/learner/",  "/general/"]

            shouldOutputTo = case value of
              Prepare  {}                      -> isAcceptor
              Proposed {}                      -> isAcceptor
              Accepted {}                      -> isLearner
              MultiPromised _ timePeriod _     -> isRelevantProposer timePeriod
              FreePromised  _ timePeriod _     -> isRelevantProposer timePeriod
              BoundPromised _ timePeriod _ _ _ -> isRelevantProposer timePeriod

            outputQueues = [ queue
                           | (queueName, (_, queue)) <- M.toList activeQueuesMap
                           , queueName /= incomingQueue
                           , shouldOutputTo queueName ]

        return $ do
          forM_ (M.keys staleQueuesMap) $ \staleQueueName ->
            logMessage receivedTime staleQueueName ("expired at " <> formatISO8601Millis staleIfNotQueriedSince)
          forM_ outputQueues $ \queue -> do
            dropRV  <- randomRIO (0.0, 100.0)
            let toMilliseconds = fromIntegral . (1000000 *)
            delayRV <- randomRIO (toMilliseconds cMinDelaySec, toMilliseconds cMaxDelaySec)
            when (dropRV >= cDropPercentage) $ void $ forkIO $ do
              threadDelay delayRV
              atomically $ writeTQueue queue value

