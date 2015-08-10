{-# LANGUAGE FlexibleContexts #-}

import Test.Hspec
import Data.Monoid
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

type ProposalId = Int
type AcceptorId = String
type Value = String

data AcceptedMessage = Accepted ProposalId AcceptorId Value
  deriving (Show, Eq)

data LearnerState = LearnerState [AcceptedMessage]

learner :: (MonadState LearnerState m, MonadWriter [String] m) => AcceptedMessage -> m ()
learner newMessage@(Accepted proposalId acceptorId value) = do
  LearnerState previousMessages <- get
  put $ LearnerState $ newMessage : previousMessages 
  forM_ previousMessages tellValueIfMatch
  where
  tellValueIfMatch (Accepted proposalId' acceptorId' _)
    | proposalId == proposalId'
    , acceptorId /= acceptorId'
    = tell [value]

    | otherwise = return ()

data PromiseType = Free | Bound ProposalId Value deriving (Show, Eq)
data PromisedMessage = Promised ProposalId AcceptorId PromiseType deriving (Show, Eq)
data ProposedMessage = Proposed ProposalId Value deriving (Show, Eq)

data ProposerState = ProposerState [PromisedMessage]

proposer :: (MonadState ProposerState m, MonadWriter [ProposedMessage] m, MonadReader Value m) => PromisedMessage -> m ()
proposer newMessage@(Promised proposalId acceptorId promiseType) = do
  ProposerState previousMessages <- get
  put $ ProposerState $ newMessage : previousMessages 
  forM_ previousMessages proposeValueIfMatch
  where
  proposeValueIfMatch (Promised proposalId' acceptorId' promiseType')
    | proposalId == proposalId'
    , acceptorId /= acceptorId'
    = do
      value <- case (promiseType, promiseType') of
        (Bound pid1 val1, Bound pid2 val2)
          | pid1 < pid2   -> return val2
          | otherwise     -> return val1
        (Bound _ val1, _) -> return val1
        (_, Bound _ val2) -> return val2
        _                 -> ask
      tell [Proposed proposalId value]

    | otherwise = return ()

main :: IO ()
main = hspec $ do
  describe "Learner" $ learnerTest
    [(Accepted 1 "rosie" "foo", [])
    ,(Accepted 2 "susan" "bar", [])
    ,(Accepted 3 "emily" "baz", [])
    ,(Accepted 3 "emily" "baz", [])
    ,(Accepted 2 "susan" "bar", [])
    ,(Accepted 2 "rosie" "bar", replicate 2 "bar")
    ,(Accepted 2 "rosie" "bar", replicate 2 "bar")
    ,(Accepted 2 "emily" "bar", replicate 4 "bar")
    ,(Accepted 2 "susan" "bar", replicate 3 "bar")
    ,(Accepted 4 "susan" "bar", [])
    ,(Accepted 4 "susan" "bar", [])
    ,(Accepted 4 "emily" "bar", replicate 2 "bar")
    ]
  describe "Proposer" $ proposerTest
    [(Promised 1 "rosie" Free, [])
    ,(Promised 3 "rosie" Free, [])
    ,(Promised 1 "emily" Free, [Proposed 1 "my value"])
    ,(Promised 1 "susan" Free, [Proposed 1 "my value", Proposed 1 "my value"])
    ,(Promised 3 "emily" (Bound 2 "another value"), [Proposed 3 "another value"])
    ,(Promised 3 "susan" (Bound 1 "yet another value"), [Proposed 3 "another value"])
    ]

learnerTest :: [(AcceptedMessage, [String])] -> SpecWith ()
learnerTest testSeq = forM_ (inits testSeq) $ \ioPairs -> it ("handles " <> show (length ioPairs) <> " messages")
  $ let inputs          =          map fst ioPairs
        expectedOutputs = concat $ map snd ioPairs
        (_, _, outputs) = runRWS (mapM_ learner inputs) () (LearnerState [])
  in outputs `shouldBe` expectedOutputs

proposerTest :: [(PromisedMessage, [ProposedMessage])] -> SpecWith ()
proposerTest testSeq = forM_ (inits testSeq) $ \ioPairs -> it ("handles " <> show (length ioPairs) <> " messages")
  $ let inputs          =          map fst ioPairs
        expectedOutputs = concat $ map snd ioPairs
        (_, _, outputs) = runRWS (mapM_ proposer inputs) "my value" (ProposerState [])
  in outputs `shouldBe` expectedOutputs

