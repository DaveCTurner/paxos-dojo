module Proposer

open Messenger

let private decideValue value promises =
    let proposedValues = 
        promises |> List.filter (fun promise -> promise.LastAcceptedValue.IsSome)

    match List.length proposedValues with
    | 0 -> value
    | _ -> 
        proposedValues 
        |> List.minBy (fun promise -> -promise.LastAcceptedTimePeriod.Value)
        |> (fun promise -> promise.LastAcceptedValue.Value)

let propose proposal url =
    let rec proposeRecursive (previousPromises:Promise list) lastProposalTimePeriod =
        
        let currentPromise = getNextPromise url
        printfn "Proposer received: \n%A" currentPromise

        match currentPromise.TimePeriod > lastProposalTimePeriod with
        | false ->
            printfn "ignoring proposal from %s as too old" currentPromise.Acceptor 
            proposeRecursive previousPromises lastProposalTimePeriod
        | true ->
            let samePeriodPromises = 
                previousPromises
                |> List.filter (fun previousPromise ->
                    previousPromise.TimePeriod = currentPromise.TimePeriod
                    && previousPromise.Acceptor <> currentPromise.Acceptor)

            let newProposalTimePeriod = 
                match List.isEmpty samePeriodPromises with
                | true -> lastProposalTimePeriod
                | false ->
                    let valueToPropose = decideValue proposal (currentPromise::samePeriodPromises)
                    printfn "** Proposer proposing ** \n Value: %s\nTimePeriod: %i" valueToPropose currentPromise.TimePeriod
                    printfn "Return code: %i" (sendProposal proposal currentPromise.TimePeriod url)
                    currentPromise.TimePeriod

            proposeRecursive (currentPromise::previousPromises) newProposalTimePeriod

    proposeRecursive [] 0L


