module ConsoleApp

open Learner
open Proposer
open Acceptor

[<EntryPoint>]
let main argv = 
    printfn "Welcome to the Paxos F# Implementation"

    learn "http://paxos.leedscodedojo.org.uk/dev/l/my-team-name"

    //propose "Dangerous Dave's Distributed Doings" "http://paxos.leedscodedojo.org.uk/dev/p/my-team-name"

    //accept "Jim" "http://paxos.leedscodedojo.org.uk/dev/a/my-team-name"

    0
