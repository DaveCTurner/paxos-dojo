{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.Timeout
import Control.Monad
import Data.Aeson
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
import Options.Applicative
import System.Random
import System.Console.ANSI
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data StaticConfig = StaticConfig
  { cPort :: Int
  } deriving (Show, Eq)

optParser :: ParserInfo StaticConfig
optParser = info (helper <*> (StaticConfig
  <$> option auto       (long "port" <> metavar "PORT" <> help "Listen for connections on port PORT")))

  (fullDesc <> progDesc "Paxos Dojo server")

type InstanceId = Integer
type AcceptorId = T.Text

data TimePeriod = SimpleTimePeriod Integer | CompoundTimePeriod [Integer]

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

instance ToJSON TimePeriod where
  toJSON (SimpleTimePeriod tp)   = toJSON tp
  toJSON (CompoundTimePeriod tp) = toJSON tp

instance FromJSON TimePeriod where
  parseJSON o = (SimpleTimePeriod   <$> parseJSON o)
            <|> (CompoundTimePeriod <$> parseJSON o)


instance Hashable TimePeriod where
  hashWithSalt s (SimpleTimePeriod tp)   = hashWithSalt s tp
  hashWithSalt s (CompoundTimePeriod tp) = hashWithSalt s tp

instance Show TimePeriod where
  show (SimpleTimePeriod tp)   = show tp
  show (CompoundTimePeriod tp) = show tp

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
  , cShowIncoming   :: Bool
  , cShowOutgoing   :: Bool
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
    , "show-incoming"    .= cShowIncoming
    , "show-outgoing"    .= cShowOutgoing
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
    <*> o .: "show-incoming"
    <*> o .: "show-outgoing"

data Status = Status Config Integer (M.Map Integer B.ByteString) [B.ByteString] [(B.ByteString, UTCTime, Bool)]

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

tpColors :: [[SGR]]
tpColors =
  [ [SetColor Foreground colIntensity col, SetConsoleIntensity weight]
  | colIntensity <- [Dull, Vivid]
  , col <- [Red, Green, Yellow, Blue, Magenta, Cyan, White]
  , weight <- [NormalIntensity, BoldIntensity]
  ]

showMaybeInstance :: Maybe InstanceId -> String
showMaybeInstance Nothing = ""
showMaybeInstance (Just i) = printf "%6s" ("[" <> show i <> "]")

showValue :: Value -> String
showValue = T.unpack . T.decodeUtf8 . BL.toStrict . encode

showAccr :: AcceptorId -> String
showAccr = (" by " ++) . T.unpack

formatForLog :: PaxosMessage -> String
formatForLog (Prepare mi tp)                = startLogLine Vivid Red    "PREP" mi       tp
formatForLog (MultiPromised  i tp a)        = startLogLine Dull Yellow  "PROM" (Just i) tp ++ showAccr a ++ " (includes future instances)"
formatForLog (FreePromised  mi tp a)        = startLogLine Dull Yellow  "PROM" mi       tp ++ showAccr a ++ " (nothing accepted yet)"
formatForLog (BoundPromised mi tp a tp' v') = startLogLine Dull Yellow  "PROM" mi       tp ++ showAccr a ++ " (last accepted " ++ showValue v' ++ " at " ++ show tp' ++ ")"
formatForLog (Proposed      mi      tp  v)  = startLogLine Vivid Yellow "PROP" mi       tp ++ " = " ++ showValue v
formatForLog (Accepted      mi    a tp  v)  = startLogLine Vivid Green  "ACCD" mi       tp ++ " = " ++ showValue v ++ showAccr a

startLogLine :: ColorIntensity -> Color -> String -> Maybe InstanceId -> TimePeriod -> String
startLogLine intensity typeColor typeName maybeInstance timePeriod = printf "%s%4s%s%s %s%10s%s"
  (setSGRCode [SetColor Foreground intensity typeColor])
  (typeName :: String)
  (setSGRCode [Reset])

  (showMaybeInstance maybeInstance)

  (setSGRCode (tpColors !! (hash timePeriod `mod` length tpColors)))
  (show timePeriod)
  (setSGRCode [Reset])

main :: IO ()
main = execParser optParser >>= \StaticConfig{..} -> do
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
    , cShowIncoming   = True
    , cShowOutgoing   = False
    }

  let logMessage :: UTCTime -> MessageDirection -> B.ByteString -> String -> IO ()
      logMessage time messageDirection queueName message = void $ forkIO $ withMVar logLock $ \_ -> do
        putStrLn $ printf "%s%-13s%s %s%-20s %c%s %s"
          (setSGRCode [ SetColor Foreground Vivid Magenta ])
          (take 13 $ drop 11 $ formatISO8601Micros time)
          (setSGRCode [Reset])

          (setSGRCode [ SetColor Foreground Vivid $ case messageDirection of Inbound -> Red; Outbound -> Green; Notification -> Yellow ])
          (T.unpack $ T.decodeUtf8 queueName)
          (case messageDirection of Inbound -> '>'; Outbound -> '<'; Notification -> '!')
          (setSGRCode [Reset])

          message

  withAsync (run cPort $ \req respond -> do
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
                responseTime <- getCurrentTime
                when cShowOutgoing $ logMessage responseTime Outbound queueName $ formatForLog value
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
                Config{..} <- atomically $ readTVar configVar
                when cShowIncoming $ logMessage now Inbound queueName $ formatForLog value
                atomically $ writeTQueue incomingQueue (queueName, now, value)
                respondEmpty

      _ -> respondBadMethod)



    $ \_ -> withAsync (forever $ do

        Config{..} <- atomically $ checking ((>0) . cNagPeriodSec) $ readTVar configVar
        threadDelay $ cNagPeriodSec * 1000000

        now <- getCurrentTime
        let timePeriod = floor $ diffUTCTime now unixEpoch
            value = Prepare Nothing $ SimpleTimePeriod timePeriod
        when cShowIncoming $ logMessage now Inbound "/nag" $ formatForLog value
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

            proposerPrefixes = ["/p/", "/proposer/", "/g/", "/general/"]
            isProposer = prefixIsOneOf proposerPrefixes
            isAcceptor = prefixIsOneOf ["/a/", "/acceptor/", "/g/", "/general/"]
            isLearner  = prefixIsOneOf ["/l/", "/learner/",  "/g/", "/general/"]

            findRelevantProposer (SimpleTimePeriod timePeriod) = do
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

            findRelevantProposer (CompoundTimePeriod [])  = return $ const False
            findRelevantProposer (CompoundTimePeriod tps) = return (`elem` [T.encodeUtf8 (prefix <> T.pack (show p)) | prefix <- proposerPrefixes])
              where p = last tps

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

