module Messenger

open HttpClient
open FSharp.Data

// JSON response parsers
type LearnerResponse = JsonProvider<"""{"timePeriod":9999999999,"value":"x","by":"x","type":"x"}""">
type ProposerResponse = JsonProvider<"""[{"type":"x","timePeriod":9999999999,"by":"x","haveAccepted":false}, {"type":"x","timePeriod":9999999999,"by":"x","lastAcceptedTimePeriod":9999999999,"lastAcceptedValue":"x"}]""", SampleIsList=true>
type AcceptorResponse = JsonProvider<"""[{"type":"x","timePeriod":9999999999}, {"type":"x","timePeriod":9999999999,"value":"x"}]""", SampleIsList=true>

// Learner types
type Acceptance = {TimePeriod:int64; Acceptor:string; Value:string}

// Proposer types
type Promise = {TimePeriod:int64; Acceptor:string; Accepted:bool option; LastAcceptedTimePeriod:int64 option; LastAcceptedValue:string option}

//Acceptor types
type Proposal = {TimePeriod:int64; Value:string}
type PreparationOrProposal = 
    | Preparation of int64
    | Proposal of Proposal

// Utlity functions
let rec private getNonEmptyResponse responseTransformer url =
    match url |> createRequest Get |> getResponseBody with
    | "" -> getNonEmptyResponse responseTransformer url // sometimes get empty responses
    | response ->
        response |> responseTransformer

let private sendRequestWithBody body url =
    createRequest Post url
    |> withBody body
    |> getResponseCode

// Learner functions
let getNextAcceptance url =
    url |> getNonEmptyResponse (fun response ->
        let responseJson = LearnerResponse.Parse( response )
        {TimePeriod = responseJson.TimePeriod; Acceptor = responseJson.By; Value = responseJson.Value})

// Proposer functions
let rec getNextPromise url =
    url |> getNonEmptyResponse (fun response ->
        let responseJson = ProposerResponse.Parse( response )
        {   TimePeriod = responseJson.TimePeriod; 
            Acceptor = responseJson.By; 
            Accepted = responseJson.HaveAccepted; 
            LastAcceptedTimePeriod = responseJson.LastAcceptedTimePeriod
            LastAcceptedValue = responseJson.LastAcceptedValue})

let sendProposal value (timePeriod:int64) url =
    url |> sendRequestWithBody (sprintf """{"type":"proposed","timePeriod":%i,"value":"%s"}""" timePeriod value)

// Acceptor functions
let rec getNextPreparationOrProposal url =
    url |> getNonEmptyResponse (fun response ->
        let responseJson = AcceptorResponse.Parse(response)
        match responseJson.Type with
        | "prepare" -> Preparation(responseJson.TimePeriod)
        | "proposed" -> Proposal({TimePeriod = responseJson.TimePeriod; Value = responseJson.Value.Value})
        | unknownType -> failwith ("unrecognised type: " + unknownType))

let sendPromiseWithoutValue acceptor (timePeriod:int64) url =
    url |> sendRequestWithBody (sprintf """{"type":"promised","timePeriod":%i,"by":"%s", "haveAccepted":false}""" timePeriod acceptor)

let sendPromiseWithValue acceptor (lastAcceptedTimePeriod:int64) lastAcceptedValue (timePeriod:int64) url =
    url |> sendRequestWithBody (sprintf """{"type":"promised","timePeriod":%i,"by":"%s", "lastAcceptedTimePeriod":%i,"lastAcceptedValue":"%s"}""" timePeriod acceptor lastAcceptedTimePeriod lastAcceptedValue)

let sendAcceptance acceptor value (timePeriod:int64) url =
    url |> sendRequestWithBody (sprintf """{"type":"accepted","timePeriod":%i,"by":"%s", "value":"%s"}""" timePeriod acceptor value)