{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (Value)
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = forever handleNextValue
  where
  uri = "http://leedscodedojo.tessanddave.com/paxos-test/a/my-initials"

  handleNextValue = do
    received <- getJSON uri
    putStrLn ("Received:   " ++ show received)
    let response = responseFor received
    putStrLn ("Responding: " ++ show response)
    postJSON uri response

  responseFor (ReceivedPrepare  timePeriod)       = SentPromise  timePeriod "alice" Free
  responseFor (ReceivedProposal timePeriod value) = SentAccepted timePeriod "alice" value

{- Basic types -}
type TimePeriod = Int
type AcceptorId = String
type Value = String
data PromiseType     = Free | Bound TimePeriod Value deriving (Show, Eq)

{- Learner message types -}
data LearnerReceived  = ReceivedAccepted TimePeriod AcceptorId Value deriving (Show, Eq)

{- Proposer message types -}
data ProposerReceived = ReceivedPromise  TimePeriod AcceptorId PromiseType deriving (Show, Eq)

data ProposerSent     = SentProposal     TimePeriod Value deriving (Show, Eq)

{- Acceptor message types -}
data AcceptorReceived = ReceivedPrepare  TimePeriod
                      | ReceivedProposal TimePeriod Value
  deriving (Show, Eq)
data AcceptorSent     = SentPromise      TimePeriod AcceptorId PromiseType 
                      | SentAccepted     TimePeriod AcceptorId Value
  deriving (Show, Eq)

getJSON :: FromJSON a => String -> IO a
getJSON uri = do
  manager <- newManager defaultManagerSettings
  req <- parseUrl uri
  runLoop req manager

  where
  runLoop req manager = do
    maybeResult <- ignoringExceptions $ withResponse req manager $ \response -> decode . BL.fromChunks <$> brConsume (responseBody response)
    maybe (threadDelay 3000000 >> runLoop req manager) return maybeResult

postJSON :: ToJSON a => String -> a -> IO ()
postJSON uri value = void $ forkIO $ do
  threadDelay 50000
  manager <- newManager defaultManagerSettings
  req <- parseUrl uri
  withResponse req { method = methodPost, requestBody = RequestBodyLBS $ encode value } manager $ \_ -> return ()

ignoringExceptions :: IO (Maybe a) -> IO (Maybe a)
ignoringExceptions
  = handleWith (ignoreException :: IOException   -> ())
  . handleWith (ignoreException :: HttpException -> ())
  where
  ignoreException = const ()
  handleWith f = handle (const (return Nothing) . f)

{- Learner comms -}

instance FromJSON LearnerReceived where
  parseJSON = withObject "LearnerReceived" $ \o -> ReceivedAccepted
    <$> o .: "timePeriod"
    <*> o .: "by"
    <*> o .: "value"

{- Proposer comms -}

instance FromJSON ProposerReceived where
  parseJSON = withObject "ProposerReceived" $ \o -> do
    haveAccepted <- fromMaybe True <$> o .:? "haveAccepted"
    ReceivedPromise
      <$> o .: "timePeriod"
      <*> o .: "by"
      <*> (if haveAccepted then Bound <$> o .: "lastAcceptedTimePeriod" <*> o .: "lastAcceptedValue" else pure Free)

instance ToJSON ProposerSent where
  toJSON (SentProposal timePeriod value) = object
    [ "type" .= ("proposed" :: String)
    , "timePeriod" .= timePeriod
    , "value" .= value
    ]

{- Acceptor comms -}

instance FromJSON AcceptorReceived where
  parseJSON = withObject "AcceptorReceived" $ \o -> do
    msgType <- o .: "type"
    if msgType == ("prepare" :: String)
      then ReceivedPrepare  <$> o .: "timePeriod"
      else ReceivedProposal <$> o .: "timePeriod" <*> o .: "value"

instance ToJSON AcceptorSent where
  toJSON (SentPromise timePeriod acceptorId Free) = object
    [ "type" .= ("promised" :: String)
    , "timePeriod" .= timePeriod
    , "by" .= acceptorId
    , "haveAccepted" .= False
    ]

  toJSON (SentPromise timePeriod acceptorId (Bound timePeriod' value')) = object
    [ "type" .= ("promised" :: String)
    , "timePeriod" .= timePeriod
    , "by" .= acceptorId
    , "lastAcceptedTimePeriod" .= timePeriod'
    , "lastAcceptedValue" .= value'
    ]


  toJSON (SentAccepted timePeriod acceptorId value) = object
    [ "type" .= ("accepted" :: String)
    , "timePeriod" .= timePeriod
    , "by" .= acceptorId
    , "value" .= value
    ]

