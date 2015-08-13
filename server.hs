{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.Timeout
import Control.Monad
import Data.Aeson hiding (Value)
import Data.Monoid
import Data.Time
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
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

main :: IO ()
main = do
  outgoingQueueByNameVar <- newTVarIO M.empty
  incomingQueue          <- newTQueueIO
  logLock                <- newMVar ()
  let logMessage :: UTCTime -> B.ByteString -> String -> IO ()
      logMessage time queueName message = void $ forkIO $ withMVar logLock $ \_ -> putStrLn
        $ printf "%-23s %-15s %s" (take 23 $ show time) (T.unpack $ T.decodeUtf8 queueName) message

  withAsync (run 24192 $ \req respond -> do
    now <- getCurrentTime
    let queueName = rawPathInfo req
    case parseMethod $ requestMethod req of
      Right GET -> do

        theQueue <- atomically $ do
          outgoingQueueByName <- readTVar outgoingQueueByNameVar
          theQueue <- case M.lookup queueName outgoingQueueByName of
            Nothing                 -> newTQueue
            Just (_, existingQueue) -> return existingQueue
          writeTVar outgoingQueueByNameVar $ M.insert queueName (now, theQueue) outgoingQueueByName
          return theQueue

        response <- timeout 10000000 $ atomically $ readTQueue theQueue

        case response of
          Nothing -> respond $ responseBuilder noContent204 [] mempty
          Just value -> do
            let valueBytes = encode (value :: PaxosMessage)
            responseTime <- getCurrentTime
            logMessage responseTime queueName $ "GET  " ++ (T.unpack $ T.decodeUtf8 $ BL.toStrict valueBytes)
            respond $ responseLBS ok200 [(hContentType, T.encodeUtf8 "application/json")] valueBytes

      Right POST -> do
        body <- strictRequestBody req
        let maybeValue = decode body
        case maybeValue of
          Nothing -> respond $ responseLBS badRequest400 [] mempty
          Just value -> do
            logMessage now queueName $ "POST " ++ (T.unpack $ T.decodeUtf8 $ BL.toStrict $ encode value)
            atomically $ writeTQueue incomingQueue (queueName, now, value :: PaxosMessage)
            respond $ responseBuilder noContent204 [] mempty

      _ -> respond $ responseLBS badRequest400 [] mempty)



    $ \_ -> forever $ join $ atomically $ do

        (queueName, receivedTime, value) <- readTQueue incomingQueue

        let staleIfNotQueriedSince = addUTCTime (-30) receivedTime
        (activeQueuesMap, staleQueuesMap) <- M.partition ((> staleIfNotQueriedSince) . fst)
            <$> readTVar outgoingQueueByNameVar
        writeTVar outgoingQueueByNameVar activeQueuesMap

        forM_ (M.elems activeQueuesMap) $ \(_, queue) -> writeTQueue queue value

        return $ do
          forM_ (M.keys staleQueuesMap) $ \staleQueueName ->
            logMessage receivedTime staleQueueName "expired"
