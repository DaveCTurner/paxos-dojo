# paxos-dojo

An idea for a Leeds Code Dojo session - implement Paxos! In fact, just the interesting core: the Synod algorithm.

There are three modules: Learner, Proposer and Acceptor. We need at least one Learner, one Proposer and exactly three Acceptors, however things get more interesting if there is more than one Learner and Proposer - it's kind of trivial if there's only one of each.

The modules will communicate via a central bus running on AWS. They will receive JSON-formatted messages with a HTTP GET, and send messages back with a HTTP POST. If there is no message to deliver within 30 seconds, the bus returns `204 No Content` and the client should immediately retry.

## Module Descriptions

### Learner

The Learner is the simplest module. It receives messages that look like this which are sent by Acceptors:

    {"type":"accepted","proposal":$PROP,"by":$NAME,"value":$VALUE}

It doesn't send any messages, but should report to the user when it has learned a value. It learns a value by receiving two messages for the same `$PROP` (an integer) but different `$NAME`s. In this case, the `$VALUE` of the two messages will always be the same, so it desn't matter which one you choose to report.

A simple Learner implementation is to keep a list of all messages received and check each new message against all the items that are already in the list, looking for pairs that match on `$PROP` but not on `$NAME`. When such a pair is found, simply print out the `$VALUE` from either message.

### Proposer

The Proposer is the next simplest module. It has a string, `$MYVALUE`, which it would like all the Learners to learn. It's recommended you include your names or a team name in this string so you can tell if this happens.

It receives messages that look like this which are sent by Acceptors:

    {"type":"promised","proposal":$PROP,"by":$NAME,"max-accepted-proposal":$MAXPROP,"max-accepted-value":$MAXVALUE}

The `max-accepted-proposal` and `max-accepted-value` fields are optional, but not independently: either both are present or both are absent.

When it has received two of these `promised` messages for the same `$PROP` (an integer) with different `$NAME`s, it should respond with a message like this:

    {"type":"proposed","proposal":$PROP,"value":$VALUE}

If neither `promised` message includes the `max-accepted-value` field, `$VALUE` should be set to `$MYVALUE`.

If one of the `promised` messages includes the `max-accepted-value:$MAXVALUE` field then `$VALUE` must be set to `$MAXVALUE`.

If both of the `promised` messages include the `max-accepted-value:$MAXVALUE` field then look at the `max-accepted-proposal:$MAXPROP` field, and set `$VALUE` to the `$MAXVALUE` of the message with the greater `$MAXPROP`. In the case of a tie, they will both be equal so it doesn't matter which one is picked.

A simple Proposer implementation is to keep a list of all messages received and check each new message against all the items that are already in the list, looking for pairs that match on `$PROP` but not on `$NAME`. When such a pair is found, the `$VALUE` can be calculated and the `proposed` message sent as described above.

### Acceptor

The Acceptor is the most complicated module. It has a name, `$NAME`, which will be agreed with the facilitator in advance as it must not clash with that of the other Acceptors.

It receives two kinds of message:

    {"type":"prepare","proposal":$PROP}
    {"type":"proposed","proposal":$PROP,"value":$VALUE}

Under the conditions set out below it may respond to these, respectively, with:

    {"type":"promised","proposal":$PROP,"by":$NAME,"max-accepted-proposal":$MAXPROP,"max-accepted-value":$MAXVALUE}
    {"type":"accepted","proposal":$PROP,"by":$NAME,"value":$VALUE}

It can send a `promised` message... TODO

It may respond

It may respond to a `prepare` message with a `promised` message, as long as it all the `and to a `proposed` message with an `accepted` message.
