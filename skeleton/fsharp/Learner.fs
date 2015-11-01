module Learner

open Messenger

let learn url =

    // get an acceptance
    getNextAcceptance url
    
    // do some stuff