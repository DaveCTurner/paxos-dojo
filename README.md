# paxos-dojo

An idea for a Leeds Code Dojo session - implement Paxos! In fact, just the
interesting core: the Synod protocol.

## Overview

The objective of the protocol is for a distributed system to reach
agreement on a single value (a string) by communicating over the network. The
clever bit is that the protocol still works even if the network is unreliable:
messages may be delayed, reordered or even dropped and the protocol still
works. The worst thing that can happen is that no agreement is reached, and
this only happens while the network is dropping messages: as soon as enough
messages start to get through again, agreement will be reached.

There are three kinds of module in the protocol, known as Learner, Proposer and
Acceptor. The intention is that each team implements one of these, although
faster teams may be able to do more than one.

We need at least one Learner, one Proposer and exactly three Acceptors, however
things get more interesting if there is more than one Learner and Proposer -
it's kind of trivial if there's only one of each.

The modules will communicate via a central bus running in the cloud. Performing
a HTTP GET will return a JSON-formatted message, if one is available. If not,
the bus will wait for a while to see if one becomes available, and eventually
return `204 No Content` if the wait is fruitless. The module should then retry
its GET.

Messages can be sent with a separate HTTP POST to the bus.

## Module Descriptions

This section describes what messages each module may send and receive.  Since
the protocol continues to work even if some messages are lost, it is ok for
modules to choose to ignore a received message, and not to send a message even
if this section permits it. On the other hand, everything goes wrong if you
send a message that isn't permitted, so it is important to err on the side of
caution. That said, if too many messages are ignored then agreement will never
be reached, so don't be too cautious!

### Learner

The Learner is the simplest module. It receives messages that look like this:

```javascript
{"type":"accepted","proposal":$PROP,"by":$NAME,"value":$VALUE}
```

It doesn't send any messages, but should report to the user when it has learned
a value. It learns a value by receiving two messages for the same `$PROP` (an
integer) but different `$NAME`s. In this case, the `$VALUE` of the two messages
will always be the same, so it desn't matter which one you choose to report.

A simple Learner implementation is to keep a list of all messages received and
check each new message against all the items that are already in the list,
looking for pairs that match on `$PROP` but not on `$NAME`. When such a pair is
found, simply print out the `$VALUE` from either message.

Here is an example of the expected behaviour:

```javascript
{"type":"accepted","proposal":1,"by":"alice","value":"value 1"}
  // no value learned - no previous messags

{"type":"accepted","proposal":2,"by":"brian","value":"value 2"}
  // no value learned - different $PROP from previous message

{"type":"accepted","proposal":2,"by":"brian","value":"value 2"}
  // no value learned - different $PROP from or same $NAME as previous messages

{"type":"accepted","proposal":2,"by":"chris","value":"value 2"}
  -> 'value 2' // value learned - same $PROP but different $NAME compared with previous message
```

There can be as many Learners running as desired. The fundamental point of Paxos is that
all values learned by all Learners are equal.

### Proposer

The Proposer is the next simplest module. It has a constant string, `$MYVALUE`,
which it would like all the Learners to learn. This string can be whatever you
want as long as it does not change once the protocol has started. It is a good
idea to include your names or a team identifier so you can track it.

It receives messages that look like this (which are sent by Acceptors):

```javascript
{"type":"promised","proposal":$PROP,"by":$NAME,"max-accepted-proposal":$MAXPROP,"max-accepted-value":$MAXVALUE}
```

The `max-accepted-proposal` and `max-accepted-value` fields are optional, but
not independently: either both are present or both are absent.

When it has received two of these `promised` messages for the same `$PROP` (an
integer) with different `$NAME`s, it should respond with a message like this:

```javascript
{"type":"proposed","proposal":$PROP,"value":$VALUE}
```

In this situation, `$VALUE` depends on the `$MAXVALUE` and `$MAXPROP` values in
the two `promised` messages as follows:

- If neither message includes the `max-accepted-value` field then use
  `$MYVALUE`, the constant string that you want everyone to learn.

- If just one of the `promised` messages includes the
  `max-accepted-value:$MAXVALUE` field then use `$MAXVALUE`.

- If both of the `promised` messages include the `max-accepted-value:$MAXVALUE`
  field then it is the `$MAXVALUE` of the message with the greater value of
`$MAXPROP`. In the case of a tie, either will do.

It must only ever send out a single `proposed` message for each `$PROP`.

A simple Proposer implementation is to keep

- a list of all messages received, and
- a list of all `$PROP` values for which `proposed` messages have been sent.

When a new message is received, ignore it if its `$PROP` value appears in the
already-sent list and otherwise check it against all the other received
messages.  If any of them match on `$PROP` but not on `$NAME` then send a
`proposed` message with `$VALUE` calculated as described above.

Here is an example of the expected behaviour:

```javascript
{"type":"promised","proposal":1,"by":"alice"}
  // nothing proposed - no previous messages

{"type":"promised","proposal":2,"by":"brian"}
  // nothing proposed - different $PROP from previous message

{"type":"promised","proposal":2,"by":"brian"}
  // nothing proposed - different $PROP from or same $NAME as previos messages

{"type":"promised","proposal":2,"by":"chris"}
  -> {"type":"proposed","proposal":2,"value":"my value"}
  // proposal made using $MYVALUE as no max-accepted-value field in any promises

{"type":"promised","proposal":2,"by":"alice"}
  // nothing proposed - proposal 2 has already been made

{"type":"promised","proposal":3,"by":"brian"}
  // nothing proposed - new promise for proposal 3

{"type":"promised","proposal":3,"by":"alice","max-accepted-proposal":1,"max-accepted-value":"alice's value"}
  -> {"type":"proposed","proposal":3,"value":"alice's value"}
  // proposal made using value from Alice's promise as it included a max-accepted-value field

{"type":"promised","proposal":4,"by":"alice","max-accepted-proposal":1,"max-accepted-value":"alice's value"}
  // nothing proposed - new promise for proposal 4

{"type":"promised","proposal":4,"by":"brian"}
  -> {"type":"proposed","proposal":4,"value":"alice's value"}
  // proposal made using value from Alice's promise as it included a max-accepted-value field

{"type":"promised","proposal":5,"by":"alice","max-accepted-proposal":1,"max-accepted-value":"alice's value"}
  // nothing proposed - new promise for proposal 5

{"type":"promised","proposal":5,"by":"brian","max-accepted-proposal":2,"max-accepted-value":"brian's value"}
  -> {"type":"proposed","proposal":5,"value":"brian's value"}
  // proposal made using value from Brian's promise as it had the greater value for max-accepted-proposal

{"type":"promised","proposal":6,"by":"brian","max-accepted-proposal":2,"max-accepted-value":"brian's value"}
  // nothing proposed - new promise for proposal 6

{"type":"promised","proposal":6,"by":"alice","max-accepted-proposal":1,"max-accepted-value":"alice's value"}
  -> {"type":"proposed","proposal":6,"value":"brian's value"}
  // proposal made using value from Brian's promise as it had the greater value for max-accepted-proposal
```

### Acceptor

The Acceptor is the most complicated module. It has a name, `$NAME` (one of
`"alice"`, `"brian"` or `"chris"`) which will be agreed in advance as it must
not clash with that of the other Acceptors. It should include this name in the
`by` field of any messages it sends.

It receives two kinds of message:

```javascript
{"type":"prepare","proposal":$PROP}
{"type":"proposed","proposal":$PROP,"value":$VALUE}
```

Under the conditions set out below it may respond to these, respectively, with:

```javascript
{"type":"promised","proposal":$PROP,"by":$NAME,"max-accepted-proposal":$MAXPROP,"max-accepted-value":$MAXVALUE}
{"type":"accepted","proposal":$PROP,"by":$NAME,"value":$VALUE}
```

A `promised` message is sent in response to the receipt of a `prepare` message.
The values of `$MAXPROP` and `$MAXVALUE` are calculated by considering the
`accepted` messages that have already been sent with a (strictly) smaller
`$PROP` value:

- If no such `accepted` message has been sent, the `max-accepted-proposal` and
  `max-accepted-value` fields must be omitted.

- If at least one `accepted` message has been sent, then `$MAXPROP` must be the
  greatest `$PROP` from such a message, and `$MAXVALUE` must be the
corresponding `$VALUE`.

An `accepted` message is sent in response to the receipt of a `proposed`
message as long as no `promised` message has already been sent with a
(strictly) greater `$PROP` value.

The obvious Acceptor implementation is to track all sent messages and apply the
logic described above when handling a newly-received message.

It is actually enough just to track the greatest `$PROP` for which an
`accepted` message has been sent (along with the corresponding `$VALUE`) and
also to track the greatest `$PROP` for which a `promised` message has been
sent. This isn't enough information to handle all possible future messages, but
in practice it works just fine. This is the recommended implementation.

Here is an example of the expected behaviour, tracking just the greatest `$PROP` values:

```javascript
{"type":"prepare","proposal":2}
  -> {"type":"promised","proposal":2,"by":"me"}
  // NB no "max-accepted-proposal" or "max-accepted-value" as nothing accepted yet

{"type":"prepare","proposal":1}
  -> {"type":"promised","proposal":1,"by":"me"}
  // ok to send out an earlier promise too

{"type":"prepare","proposal":2}
  -> {"type":"promised","proposal":2,"by":"me"}
  // ok to send out a duplicate promise

{"type":"proposed","proposal":1,"value":"value 1"}
  // have promised not to accept proposals < 2 so do not respond to this

{"type":"proposed","proposal":2,"value":"value 2"}
  -> {"type":"accepted","proposal":2,"by":"me","value":"value 2"}
  // ok to accept this proposal as it is >= 2 so consistent with all promises

{"type":"prepare","proposal":1}
  // no response as have accepted a >= proposal

{"type":"prepare","proposal":2}
  // no response as have accepted a >= proposal

{"type":"prepare","proposal":3}
  -> {"type":"promised","proposal":3,"by":"me","max-accepted-proposal":2,"max-accepted-value":"value 2"}
  // send out a promise, but now includes "max-accepted-proposal" and "max-accepted-value"

{"type":"proposed","proposal":4,"value":"value 4"}
  -> {"type":"accepted","proposal":4,"by":"me","value":"value 4"}
  // ok to accept this proposal as it is >= 3 so consistent with all promises

{"type":"proposed","proposal":4,"value":"different value 4"}
  // have already accepted proposal 4, so do not accept it again. Technically,
  // could re-send:
  //     {"type":"accepted","proposal":4,"by":"me","value":"value 4"}
  // (NB the value must be what was originally accepted) but this is
  // unnecessary and makes the implementation more complicated.

{"type":"proposed","proposal":3,"value":"value 3"}
  // have already accepted proposal 4, so do not accept earlier-numbered
  // proposals. Technically, could send:
  //     {"type":"accepted","proposal":3,"by":"me","value":"value 3"}
  // as proposal 3 has not been accepted, but this is unnecessary and makes the
  // implementation more complicated.
```
