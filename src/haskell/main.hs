{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (Value)
import Data.Hashable
import Data.Int
import Data.List
import Data.Maybe
import Data.Tuple.Utils
import Network.HTTP.Client
import Network.HTTP.Types
import Options.Applicative
import System.Posix
import Test.Hspec
import Text.Printf
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S

data Config = Config
  { cBaseURI :: String
  , cAcceptorNames :: [String]
  , cProposedValues :: [String]
  } deriving (Show, Eq)

optParser :: ParserInfo Config
optParser = info (helper <*> (Config
  <$>       strOption   (long "base-uri" <> metavar "URI"   <> help "Use messaging server with this base URI")
  <*> many (strOption   (long "acceptor" <> metavar "NAME"  <> help "Run this acceptor (may be repeated)"))
  <*> many (strOption   (long "propose"  <> metavar "VALUE" <> help "Value to propose (may be repeated)"))))

  (fullDesc <> progDesc "Paxos Dojo client")

main :: IO ()
main = execParser optParser >>= \Config{..} -> do
  logLock <- newMVar ()
  CPid processID <- getProcessID
  let uriSource = UriSource cBaseURI processID

  forM_ cAcceptorNames  $ forkIO . runAcceptor uriSource (AcceptorState Nothing Free)
  forM_ cProposedValues $ forkIO . runProposer uriSource (ProposerState S.empty [])

  runLearner uriSource (LearnerState []) logLock

data UriSource = UriSource String Int32

learnerUri :: UriSource -> String
learnerUri (UriSource base pid) = printf "%s/l/dt-hs-%05d" base pid

proposerUri :: UriSource -> String -> String
proposerUri (UriSource base pid) val = printf "%s/p/dt-hs-%05d-%05d" base pid (hash val `mod` 10000)

acceptorUri :: UriSource -> String -> String
acceptorUri (UriSource base pid) name = printf "%s/a/dt-hs-%05d-%s" base pid name

type TimePeriod = Int
type AcceptorId = String
type Value = String

data PrepareMessage = Prepare TimePeriod
  deriving (Show, Eq)

data AcceptedMessage = Accepted TimePeriod AcceptorId Value
  deriving (Show, Eq)

data LearnerState = LearnerState [AcceptedMessage]

type LearnedValue = (TimePeriod, String)

learner :: (MonadState LearnerState m, MonadWriter [LearnedValue] m) => AcceptedMessage -> m ()
learner newMessage@(Accepted timePeriod acceptorId value) = do
  LearnerState previousMessages <- get
  put $ LearnerState $ newMessage : previousMessages
  when (any isMatch previousMessages) $ tell [(timePeriod, value)]

  where isMatch (Accepted timePeriod' acceptorId' _)
          | timePeriod /= timePeriod' = False
          | acceptorId == acceptorId' = False
          | otherwise                 = True

data PromiseType = Free | Bound TimePeriod Value deriving (Show, Eq)
data PromisedMessage = Promised TimePeriod AcceptorId PromiseType deriving (Show, Eq)
data ProposedMessage = Proposed TimePeriod Value deriving (Show, Eq)

data ProposerState = ProposerState (S.Set TimePeriod) [PromisedMessage]

proposer :: (MonadState ProposerState m, MonadWriter [ProposedMessage] m, MonadReader Value m, Functor m) => PromisedMessage -> m ()
proposer newMessage@(Promised timePeriod acceptorId promiseType) = do
  ProposerState proposalsMade previousMessages <- get
  unless (S.member timePeriod proposalsMade) $ do
    put $ ProposerState proposalsMade $ newMessage : previousMessages
    void $ runMaybeT $ forM_ previousMessages proposeValueIfMatch

  where
  proposeValueIfMatch (Promised timePeriod' acceptorId' promiseType') = when isMatch $ do
      value <- case (promiseType, promiseType') of
        (Bound pid1 val1, Bound pid2 val2)
          | pid1 < pid2   -> return val2
          | otherwise     -> return val1
        (Bound _ val1, _) -> return val1
        (_, Bound _ val2) -> return val2
        _                 -> ask
      tell [Proposed timePeriod value]
      modify $ \(ProposerState proposalsMade msgs) -> ProposerState (S.insert timePeriod proposalsMade) msgs
      mzero
    where isMatch
            | timePeriod /= timePeriod' = False
            | acceptorId == acceptorId' = False
            | otherwise                 = True

data AcceptorState = AcceptorState (Maybe TimePeriod) PromiseType

acceptor
  :: (MonadState AcceptorState m, MonadWriter [Either PromisedMessage AcceptedMessage] m, MonadReader AcceptorId m, Functor m)
  => Either PrepareMessage ProposedMessage -> m ()

acceptor (Left (Prepare timePeriod)) = do
  AcceptorState maybeMaxPromised promiseType <- get
  case promiseType of
    Bound timePeriod' _ | timePeriod <= timePeriod' -> return ()
    _ -> do
      myName <- ask
      tell [Left $ Promised timePeriod myName promiseType]
      put $ AcceptorState (max maybeMaxPromised $ Just timePeriod) promiseType

acceptor (Right (Proposed timePeriod value)) = do
  AcceptorState maybeMaxPromised promiseType <- get
  when (maybeMaxPromised <= Just timePeriod) $ case promiseType of
    Bound timePeriod' _ | timePeriod <= timePeriod' -> return ()
    _ -> do
      myName <- ask
      tell [Right $ Accepted timePeriod myName value]
      put $ AcceptorState maybeMaxPromised $ Bound timePeriod value

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

runLearner :: UriSource -> LearnerState -> MVar () -> IO a
runLearner uriSource s0 logLock = go s0
  where
  uri = learnerUri uriSource
  go s = do
    acceptedMessage <- getJSON uri
    let ((),s',outputs) = runRWS (learner acceptedMessage) () s
    forM_ outputs $ \(timePeriod, learnedValue) -> withMVar logLock $ \_
      -> putStrLn $ printf "%10d: learned value '%s'" timePeriod learnedValue
    go s'

instance FromJSON AcceptedMessage where
  parseJSON = withObject "AcceptedMessage" $ \o -> Accepted
    <$> o .: "timePeriod"
    <*> o .: "by"
    <*> o .: "value"

instance FromJSON PromisedMessage where
  parseJSON = withObject "PromisedMessage" $ \o -> do
    haveAccepted <- fromMaybe True <$> o .:? "haveAccepted"
    Promised
      <$> o .: "timePeriod"
      <*> o .: "by"
      <*> (if haveAccepted then Bound <$> o .: "lastAcceptedTimePeriod" <*> o .: "lastAcceptedValue" else pure Free)

instance ToJSON ProposedMessage where
  toJSON (Proposed timePeriod value) = object
    [ "type" .= ("proposed" :: String)
    , "timePeriod" .= timePeriod
    , "value" .= value
    ]

runProposer :: UriSource -> ProposerState -> String -> IO a
runProposer uriSource s0 value = go s0
  where
  uri = proposerUri uriSource value
  go s = do
    promisedMessage <- getJSON uri
    let ((),s',outputs) = runRWS (proposer promisedMessage) value s
    forM_ outputs $ postJSON uri
    go s'

runAcceptor :: UriSource -> AcceptorState -> String -> IO a
runAcceptor uriSource s0 name = go s0
  where
  uri = acceptorUri uriSource name
  go s = do
    acceptorReceivedMessage <- getJSON uri
    let msg' = case acceptorReceivedMessage of ARLeft m -> Left m; ARRight m -> Right m
    let ((),s',outputs) = runRWS (acceptor msg') name s
    forM_ outputs $ postJSON uri . either ASLeft ASRight
    go s'

data AcceptorReceived = ARLeft PrepareMessage | ARRight ProposedMessage

data AcceptorSent = ASLeft PromisedMessage | ASRight AcceptedMessage

instance FromJSON AcceptorReceived where
  parseJSON = withObject "AcceptorReceived" $ \o -> do
    msgType <- o .: "type"
    if msgType == ("prepare" :: String)
      then ARLeft  <$> (Prepare <$> o .: "timePeriod")
      else ARRight <$> (Proposed <$> o .: "timePeriod" <*> o .: "value")

instance ToJSON AcceptorSent where
  toJSON (ASLeft (Promised timePeriod acceptorId Free)) = object
    [ "type" .= ("promised" :: String)
    , "timePeriod" .= timePeriod
    , "by" .= acceptorId
    , "haveAccepted" .= False
    ]

  toJSON (ASLeft (Promised timePeriod acceptorId (Bound timePeriod' value'))) = object
    [ "type" .= ("promised" :: String)
    , "timePeriod" .= timePeriod
    , "by" .= acceptorId
    , "lastAcceptedTimePeriod" .= timePeriod'
    , "lastAcceptedValue" .= value'
    ]

  toJSON (ASRight (Accepted timePeriod acceptorId value)) = object
    [ "type" .= ("accepted" :: String)
    , "timePeriod" .= timePeriod
    , "by" .= acceptorId
    , "value" .= value
    ]

test :: IO ()
test = hspec $ do
  describe "Learner" $ learnerTest
    [(Accepted 1 "alice" "foo", [],          "should learn nothing with the first message")
    ,(Accepted 2 "brian" "bar", [],          "should learn nothing with a message for a different proposal")
    ,(Accepted 2 "brian" "bar", [],          "should learn nothing from a duplicate of the previous message")
    ,(Accepted 2 "chris" "bar", [(2,"bar")], "should learn a value from another message for proposal 2")
    ]

  describe "Proposer" $ proposerTest
    [(Promised 1 "alice" Free,                      [], "should propose nothing from the first message")

    ,(Promised 2 "brian" Free,                      [], "should propose nothing from a message for a different proposal")
    ,(Promised 2 "brian" Free,                      [], "should propose nothing from a duplicate of the previous message")
    ,(Promised 2 "chris" Free,                      [Proposed 2 "my value"],
                                "should propose 'my value' with another promise")
    ,(Promised 2 "alice" Free,                      [], "should ignore another message for a proposal that's already been made")

    ,(Promised 3 "brian" Free,                      [], "should propose nothing from the first message of proposal 3")
    ,(Promised 3 "alice" (Bound 1 "alice's value"), [Proposed 3 "alice's value"],
                                "should propose the value given in the second promise")

    ,(Promised 4 "alice" (Bound 1 "alice's value"), [], "should propose nothing from the first message of proposal 4")
    ,(Promised 4 "brian" Free,                      [Proposed 4 "alice's value"],
                                "should propose the value given in the first promise")

    ,(Promised 5 "alice" (Bound 1 "alice's value"), [], "should propose nothing from the first message of proposal 5")
    ,(Promised 5 "brian" (Bound 2 "brian's value"), [Proposed 5 "brian's value"],
                                "should propose the value given in the second promise as it has the higher-numbered value")

    ,(Promised 6 "brian" (Bound 2 "brian's value"), [],  "should propose nothing from the first message of proposal 6")
    ,(Promised 6 "alice" (Bound 1 "alice's value"), [Proposed 6 "brian's value"],
                                "should propose the value given in the first promise as it has the higher-numbered value")
    ]

  describe "Acceptor" $ acceptorTest
    [(Left $ Prepare 2, [Left $ Promised 2 "me" Free], "should emit a promise for the first message")
    ,(Left $ Prepare 1, [Left $ Promised 1 "me" Free], "should emit a promise for a lower-numbered proposal too")
    ,(Left $ Prepare 2, [Left $ Promised 2 "me" Free], "should emit a promise for a duplicate proposal")
    ,(Right $ Proposed 1 "a value", [], "should not accept a proposal for a lower proposal number")
    ,(Right $ Proposed 2 "another value", [Right $ Accepted 2 "me" "another value"], "should not accept a proposal for a lower proposal number")
    ,(Left $ Prepare 1, [], "should not emit a promise now that a > proposal has been accepted")
    ,(Left $ Prepare 2, [], "should not emit a promise now that a == proposal has been accepted")
    ,(Left $ Prepare 3, [Left $ Promised 3 "me" (Bound 2 "another value")],
        "should emit a promise for a > proposal")
    ,(Right $ Proposed 4 "yet another value", [Right $ Accepted 4 "me" "yet another value"],
        "should emit a promise for a > proposal")
    ,(Right $ Proposed 4 "yet another value", [], "should not accept the same proposal again")
    ,(Right $ Proposed 3 "yet another value", [], "should not accept an earlier proposal either")
    ]

learnerTest :: [(AcceptedMessage, [(TimePeriod, String)], String)] -> SpecWith ()
learnerTest = generalTest learner () (LearnerState [])

proposerTest :: [(PromisedMessage, [ProposedMessage], String)] -> SpecWith ()
proposerTest = generalTest proposer "my value" (ProposerState S.empty [])

acceptorTest :: [(Either PrepareMessage ProposedMessage, [Either PromisedMessage AcceptedMessage], String)] -> SpecWith ()
acceptorTest = generalTest acceptor "me" (AcceptorState Nothing Free)

generalTest :: (Show output, Eq output)
  => (input -> RWS env [output] state b) -> env -> state -> [(input, [output], String)] -> SpecWith ()
generalTest stateMachine environment initialState testSeq = forM_ (inits testSeq) $ \ioPairs ->
  let inputs          =          map fst3 ioPairs
      expectedOutputs = concat $ map snd3 ioPairs
      worksAsExpected = last $ "should emit nothing from no messages" : map thd3 ioPairs
      (_, _, outputs) = runRWS (mapM_ stateMachine inputs) environment initialState
  in it worksAsExpected $ outputs `shouldBe` expectedOutputs
