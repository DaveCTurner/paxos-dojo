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
import Data.Hashable
import Data.List (findIndex)
import Data.Maybe
import Data.Monoid
import Data.Time
import Data.Time.ISO8601
import Network.HTTP.Types hiding (Status)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Random
import System.Console.ANSI
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
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
    , "haveAccepted" .= False
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
      PromisedType -> do
        includesGreaterInstances <- fromMaybe False <$> o .:? "includesGreaterInstances"
        haveAccepted             <- fromMaybe True  <$> o .:? "haveAccepted"
        case (includesGreaterInstances, haveAccepted) of
             (True,                     _           ) -> MultiPromised
                                                          <$> o .: "instance"
                                                          <*> timePeriod
                                                          <*> acceptorId
             (_,                        False       ) -> FreePromised
                                                          <$> maybeInstance
                                                          <*> timePeriod
                                                          <*> acceptorId
             _                                        -> BoundPromised
                                                          <$> maybeInstance
                                                          <*> timePeriod
                                                          <*> acceptorId
                                                          <*> o .: "lastAcceptedTimePeriod"
                                                          <*> o .: "lastAcceptedValue"

data Config = Config
  { cGetTimeoutSec  :: Integer
  , cQueueExpirySec :: Integer
  , cDropPercentage :: Double
  , cMinDelaySec    :: Int
  , cMaxDelaySec    :: Int
  , cNagPeriodSec   :: Int
  , cPartitionedAcceptors :: [T.Text]
  }

instance ToJSON Config where
  toJSON Config{..} = object
    [ "get-timeout-sec"  .= cGetTimeoutSec
    , "queue-expiry-sec" .= cQueueExpirySec
    , "drop-percentage"  .= cDropPercentage
    , "min-delay-sec"    .= cMinDelaySec
    , "max-delay-sec"    .= cMaxDelaySec
    , "nag-period-sec"   .= cNagPeriodSec
    , "partitioned-acceptors" .= cPartitionedAcceptors
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config
    <$> o .: "get-timeout-sec"
    <*> o .: "queue-expiry-sec"
    <*> o .: "drop-percentage"
    <*> o .: "min-delay-sec"
    <*> o .: "max-delay-sec"
    <*> o .: "nag-period-sec"
    <*> o .: "partitioned-acceptors"

data Status = Status Config Integer (M.Map TimePeriod B.ByteString) [B.ByteString] [(B.ByteString, UTCTime, Bool)]

instance ToJSON Status where
  toJSON (Status config minTimePeriod proposersByTimePeriod nextProposers queues) = object
    [ "config" .= config
    , "min-time-period" .= minTimePeriod
    , "next-proposers" .= map T.decodeUtf8 nextProposers
    , "proposers-by-time-period" .= object [T.pack (show k) .= T.decodeUtf8 v | (k,v) <- M.toList proposersByTimePeriod]
    , "queues" .= map (\(queueName, lastGetTime, isEmpty) -> object
        [ "queue" .= T.decodeUtf8 queueName
        , "last-get-time" .= formatISO8601Micros lastGetTime
        , "is-empty" .= isEmpty
        ]) queues
    ]

unixEpoch :: UTCTime
unixEpoch = UTCTime (fromGregorian 1970 01 01) 0

checking :: (a -> Bool) -> STM a -> STM a
checking p go = do
  result <- go
  check $ p result
  return result

data MessageDirection
  = Inbound
  | Outbound
  | Notification

main :: IO ()
main = do
  outgoingQueueByNameVar   <- newTVarIO M.empty
  incomingQueue            <- newTQueueIO
  logLock                  <- newMVar ()
  minTimePeriodVar         <- newTVarIO 0
  proposersByTimePeriodVar <- newTVarIO M.empty
  nextProposersVar         <- newTVarIO []
  configVar                <- newTVarIO $ Config
    { cGetTimeoutSec  = 10
    , cQueueExpirySec = 60
    , cDropPercentage = 0
    , cMinDelaySec    = 0
    , cMaxDelaySec    = 0
    , cNagPeriodSec   = 5
    , cPartitionedAcceptors = []
    }

  let logMessage :: UTCTime -> MessageDirection -> B.ByteString -> String -> IO ()
      logMessage time messageDirection queueName message = void $ forkIO $ withMVar logLock $ \_ -> do
        putStrLn $ printf "%s%-13s%s %s%c%-15s%s %s"
          (setSGRCode [ SetColor Foreground Vivid Magenta ])
          (take 13 $ drop 11 $ formatISO8601Micros time)
          (setSGRCode [Reset])

          (setSGRCode [ SetColor Foreground Vivid $ case messageDirection of Inbound -> Red; Outbound -> Green; Notification -> Yellow ])
          (case messageDirection of Inbound -> '<'; Outbound -> '>'; Notification -> '!')
          (T.unpack $ T.decodeUtf8 queueName)
          (setSGRCode [Reset])

          message

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
                     <*> readTVar minTimePeriodVar
                     <*> readTVar proposersByTimePeriodVar
                     <*> readTVar nextProposersVar
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
                logMessage responseTime Outbound queueName $ T.unpack $ T.decodeUtf8 $ BL.toStrict valueBytes
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
                logMessage now Inbound queueName $ T.unpack $ T.decodeUtf8 $ BL.toStrict $ encode value
                atomically $ writeTQueue incomingQueue (queueName, now, value :: PaxosMessage)
                respondEmpty

      _ -> respondBadMethod)



    $ \_ -> withAsync (forever $ do

        nagPeriodSec <- atomically $ checking (>0) $ cNagPeriodSec <$> readTVar configVar
        threadDelay $ nagPeriodSec * 1000000

        now <- getCurrentTime
        let timePeriod = floor $ diffUTCTime now unixEpoch
            value = Prepare Nothing timePeriod
        logMessage now Inbound "/nag" $ T.unpack $ T.decodeUtf8 $ BL.toStrict $ encode value
        atomically $ do
          let minTimePeriod = timePeriod - 300
          modifyTVar minTimePeriodVar $ max minTimePeriod
          modifyTVar proposersByTimePeriodVar $ snd . M.split minTimePeriod
          writeTQueue incomingQueue ("/nag", now, value))

    $ \_ -> forever $ join $ atomically $ do

        (incomingQueueName, receivedTime, value) <- readTQueue incomingQueue
        Config{..} <- readTVar configVar

        let staleIfNotQueriedSince = addUTCTime (fromIntegral $ negate cQueueExpirySec) receivedTime
        (activeQueuesMap, staleQueuesMap) <- M.partition ((> staleIfNotQueriedSince) . fst)
            <$> readTVar outgoingQueueByNameVar
        writeTVar outgoingQueueByNameVar activeQueuesMap

        let prefixIsOneOf prefixes qn = or [ B.isPrefixOf (T.encodeUtf8 prefix) qn | prefix <- prefixes ]

            isProposer = prefixIsOneOf ["/proposer/", "/general/"]
            isAcceptor = prefixIsOneOf ["/acceptor/", "/general/"]
            isLearner  = prefixIsOneOf ["/learner/",  "/general/"]

            findRelevantProposer timePeriod = do
              minTimePeriod <- readTVar minTimePeriodVar
              if timePeriod <= minTimePeriod
                then return $ const False
                else do
                  proposersByTimePeriod <- readTVar proposersByTimePeriodVar
                  isRelevantProposer <- case M.lookup timePeriod proposersByTimePeriod of
                    Just proposer -> return (== proposer)

                    Nothing -> do
                      waitingProposers <- readTVar nextProposersVar
                      let activeProposers = filter isProposer $ M.keys activeQueuesMap
                          nextProposers = if null waitingProposers then activeProposers else waitingProposers

                      case nextProposers of
                        [] -> return $ const False
                        (proposer:_) -> do
                          writeTVar nextProposersVar $ filter (/= proposer) nextProposers
                          writeTVar proposersByTimePeriodVar $ M.insert timePeriod proposer proposersByTimePeriod
                          return (== proposer)

                  let isInCorrectPartition = case findIndex (\a -> T.encodeUtf8 a == incomingQueueName) cPartitionedAcceptors of
                        Nothing -> const True
                        Just partition -> \p -> mod (hash p) (length cPartitionedAcceptors) == partition

                  return $ \p -> isRelevantProposer p && isInCorrectPartition p

        shouldOutputTo <- case value of
          Prepare  {}                      -> return isAcceptor
          Proposed {}                      -> return $ let partitionedAcceptorCount = length cPartitionedAcceptors in
            if partitionedAcceptorCount == 0
              then isAcceptor
              else \a -> isAcceptor a && a == T.encodeUtf8 (cPartitionedAcceptors !! (mod (hash incomingQueueName) partitionedAcceptorCount))

          Accepted {}                      -> return isLearner
          MultiPromised _ timePeriod _     -> findRelevantProposer timePeriod
          FreePromised  _ timePeriod _     -> findRelevantProposer timePeriod
          BoundPromised _ timePeriod _ _ _ -> findRelevantProposer timePeriod

        let outputQueues = [ queue
                           | (queueName, (_, queue)) <- M.toList activeQueuesMap
                           , queueName /= incomingQueueName
                           , shouldOutputTo queueName ]

        return $ do
          forM_ (M.keys staleQueuesMap) $ \staleQueueName ->
            logMessage receivedTime Notification staleQueueName ("expired at " <> formatISO8601Millis staleIfNotQueriedSince)
          forM_ outputQueues $ \queue -> do
            dropRV  <- randomRIO (0.0, 100.0)
            let toMilliseconds = fromIntegral . (1000000 *)
            delayRV <- randomRIO (toMilliseconds cMinDelaySec, toMilliseconds cMaxDelaySec)
            when (dropRV >= cDropPercentage) $ void $ forkIO $ do
              threadDelay delayRV
              atomically $ writeTQueue queue value

