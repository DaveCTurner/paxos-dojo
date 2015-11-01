module Learner

open Messenger

let learn url =

    let rec learnRecursive (previousAcceptances:Acceptance list) =
        
        let currentAcceptance = getNextAcceptance url
        printfn "Learner received: \n%A" currentAcceptance

        let matchFound = 
            previousAcceptances
            |> List.exists (fun previousAcceptance ->
                previousAcceptance.TimePeriod = currentAcceptance.TimePeriod
                && previousAcceptance.Acceptor <> currentAcceptance.Acceptor)

        if matchFound then printfn "** Learner learned ** \n%A" currentAcceptance

        learnRecursive (currentAcceptance::previousAcceptances)

    learnRecursive []