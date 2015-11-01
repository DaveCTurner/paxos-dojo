module Acceptor

open Messenger

let accept acceptorName url =

    /// get a preparation or proposal
    getNextPreparationOrProposal url

    // do some stuff

    // maybe send some stuff
    sendPromiseWithoutValue "tim" 123L url
    sendPromiseWithValue "jim" 123L "hi mum" 456L url
    sendAcceptance "kim" "hi mum" 123L url
