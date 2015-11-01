module Proposer

open Messenger

let propose proposal url =

    // get a promise
    getNextPromise url
        
    // do some stuff

    // maybe send a proposal
    sendProposal proposal 123L url


