module Acceptor

open Messenger

let accept name url =

    let rec acceptRecursive() =
        
        let currentMessage = getNextPreparationOrProposal url
        printfn "Acceptor received: \n%A" currentMessage

        // do some stuff

        printfn "sendPromiseWithoutValue: %i" (sendPromiseWithoutValue "tim" 123L url)
        printfn "sendPromiseWithValue: %i" (sendPromiseWithValue "jim" 123L "hi" 456L url)
        printfn "sendAcceptance: %i" (sendAcceptance "kim" "mum" 123L url)

        acceptRecursive()

    acceptRecursive()