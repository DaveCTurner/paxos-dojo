# paxos-dojo

An idea for a Leeds Code Dojo session - implement Paxos! In fact, just the
interesting core: the Synod protocol.

## Introduction

Synod is a protocol for reaching agreement on a single value in a distributed
system. Grant suggests we should use it to agree on a name for our new startup,
so get thinking of a good name to propose!

The thing about distributed systems is that you have to assume the individual
bits of the system are rather unreliable. In more detail:

- modules may fail (i.e. simply stop responding to messages) at any time
- modules may take arbitrarily long to respond
- messages may be dropped in transit (i.e. there is no guarantee that a sent
  message is ever received)
- messages may be delayed in transit and may not be received in the order in
  which they were sent

For many years it was believed that with all these things that could go wrong
it wouldn't be possible to reliably do anything at all. This protocol was
actually discovered in the process of trying to prove that no such protocol
could exist.

Of course if things go _too_ wrong then you can't really hope to achieve
anything, but this protocol guarantees that the worst thing that can happen is
that no agreement is reached, and this only happens while

- the network is dropping too many messages
- messages are being delayed too heavily in transit, or
- too many modules have failed

All of these problems tend to be transient: networks are eventually fixed and
failed modules are eventually replaced, and when the situation returns to
normal the protocol carries on and reaches agreement.

The system is unreliable but not actively malicious:

- modules all obey the protocol correctly
- messages may not be altered in transit
- message delivery is all-or-nothing

## Overview

There are four kinds of module in the protocol:

- Nag
- Proposer
- Acceptor and
- Learner

All the Nag does is periodically broadcast a message indicating the current
time, which is rather boring so has already been done. The intention is that
each team implements one of the other three modules. We need at least one
Learner, one Proposer and exactly three Acceptors:

<img src='diagrams/001-setup.png' width='400px' />
 
When everything is working smoothly, the protocol runs as follows. First, the
Nag broadcasts the current time period to the Acceptors:

<img src='diagrams/002-prepare.png' width='400px' />

When each Acceptor receives this broadcast, it sends a `promised` message to
the Proposer, promising that in future it won't accept any proposals made
before the current time period.

<img src='diagrams/003-promise.png' width='400px' />

When the Proposer receives these `promised` messages from a majority of
Acceptors (i.e. two of them), it proposes a value to agree upon in the current
time period by broadcasting a `proposed` message back to all the Acceptors.

<img src='diagrams/004-propose.png' width='400px' />

Since the Acceptors have not made any further promises, they all accept the
proposed value and send `accepted` messages to the Learner. Once the Learner
receives two such `accepted` messages for the same time period, it has learned
that this value is the one that the system has agreed upon.

<img src='diagrams/005-accept.png' width='400px' />

There has to be exactly three Acceptors, but it's much more interesting if
there is more than one Learner and Proposer:

- With one Proposer there is only one value to be proposed (so eventual
  consensus is guaranteed) and the system will stop working if this Proposer
fails.

- With one Learner there is only one place where a value is learned (so
  eventual consensus is guaranteed) and the system will stop working if this
Learner fails.

## Module Descriptions

For the sake of interoperability, in our implementation the modules communicate
with each other using JSON-formatted messages. This section gives the details
of the messages each module may send and receive.

Since the protocol continues to work even if some messages are lost, it is ok
for modules to choose to ignore a received message, and not to send a message
even if this section permits it. On the other hand, everything goes wrong if
you send a message that isn't permitted, so it is important to err on the side
of caution. That said, if too many messages are ignored then agreement will
never be reached, so don't be too cautious!

### Learner

The Learner is the simplest module. Roughly speaking, it collects messages
indicating that certain values have been accepted and when it receives these
messages from a majority of Acceptors (i.e. two of them) it learns that
consensus has been reached.

More precisely, it receives messages that look like this:

```javascript
{"type":"accepted","timePeriod":$TIMEPERIOD,"by":$NAME,"value":$VALUE}
```

It doesn't send any messages, but should report to the user when it has learned
a value. It learns a value by receiving two messages for the same `$TIMEPERIOD`
(a positive integer) but different `$NAME`s (which are strings). In this case,
the `$VALUE` of the two messages will always be the same, so it desn't matter
which one you choose to report.

A simple Learner implementation is to keep a list of all messages received and
check each new message against all the items that are already in the list,
looking for pairs that match on `$TIMEPERIOD` but not on `$NAME`. When such a
pair is found, simply print out the `$VALUE` from either message.

Here is an example of the expected behaviour:

```javascript
{"type":"accepted","timePeriod":1,"by":"alice","value":"value 1"}
  // no value learned - no previous messags

{"type":"accepted","timePeriod":2,"by":"brian","value":"value 2"}
  // no value learned - different $TIMEPERIOD from previous message

{"type":"accepted","timePeriod":2,"by":"brian","value":"value 2"}
  // no value learned - same $NAME as previous messages

{"type":"accepted","timePeriod":2,"by":"chris","value":"value 2"}
  -> 'value 2' // value learned - same $TIMEPERIOD but different $NAME compared with previous message
```

### Proposer

The Proposer is the next simplest module. Its job is to propose ideas for the
name of our startup. It knows your idea, which we'll call `$MYVALUE` here.
Roughly speaking, it proposes `$MYVALUE` if it hasn't receieved any other
ideas, but in the interests of harmony it prefers to agree with other
proposers' ideas when it hears about them. It also only makes proposals when
they have a good chance of being accepted by a majority of Acceptors. It is, in
short, a little bit of a wimp.

More precisely, it receives messages that look like one of these (which are
sent by Acceptors):

```javascript
{"type":"promised","timePeriod":$TIMEPERIOD,"by":$NAME}
{"type":"promised","timePeriod":$TIMEPERIOD,"by":$NAME,"lastAcceptedTimePeriod":$LATP,"lastAcceptedValue":$LAV}
```

In other words, the `lastAcceptedTimePeriod` and `lastAcceptedValue` fields are
optional, but not independently: either both are present or both are absent.

When it has received two of these `promised` messages for the same
`$TIMEPERIOD` (a positive integer) with different `$NAME`s (which are strings),
it should respond with a message like this:

```javascript
{"type":"proposed","timePeriod":$TIMEPERIOD,"value":$VALUE}
```

In this situation, `$VALUE` depends on the `$LATP` and `$LAV` values in
the two `promised` messages as follows:

- If neither message includes these values then use `$MYVALUE`.

- If just one of the `promised` messages includes these values then use its
  `$LAV`.

- If both of the `promised` messages include these values then use the most
  recent `$LAV` (i.e. the one with the greater value for `$LATP`). In the case
of a tie, either will do.

It must only ever send out a single `proposed` message for each `$TIMEPERIOD`.

A simple Proposer implementation is to keep

- a list of all messages received, and
- the latest `$TIMEPERIOD` for which a `proposed` message have been sent.

When a new message is received, ignore it if its `$TIMEPERIOD` value is no
greater than the latest-proposed `$TIMEPERIOD` and otherwise check it against
all the other received messages. If any of them match on `$TIMEPERIOD` but not
on `$NAME` then take the first one and send a `proposed` message with `$VALUE`
calculated as described above.

Here is an example of the expected behaviour:

```javascript
{"type":"promised","timePeriod":1,"by":"alice"}
  // nothing proposed - no previous messages

{"type":"promised","timePeriod":2,"by":"brian"}
  // nothing proposed - different $TIMEPERIOD from previous message

{"type":"promised","timePeriod":2,"by":"brian"}
  // nothing proposed - different $TIMEPERIOD from first message and same $NAME as second message

{"type":"promised","timePeriod":2,"by":"chris"}
  -> {"type":"proposed","timePeriod":2,"value":"my awesome startup name"}
  // proposal made using $MYVALUE as no $LAV given in any promises

{"type":"promised","timePeriod":2,"by":"alice"}
  // nothing proposed - have already made a proposal for time period 2

{"type":"promised","timePeriod":3,"by":"brian"}
  // nothing proposed - different $TIMEPERIOD from all earlier messages

{"type":"promised","timePeriod":3,"by":"alice","lastAcceptedTimePeriod":1,"lastAcceptedValue":"AliceCo"}
  -> {"type":"proposed","timePeriod":3,"value":"AliceCo"}
  // proposal made using $LAV from Alice's promise as it included a lastAcceptedValue field

{"type":"promised","timePeriod":4,"by":"alice","lastAcceptedTimePeriod":1,"lastAcceptedValue":"AliceCo"}
  // nothing proposed - different $TIMEPERIOD from all earlier messages

{"type":"promised","timePeriod":4,"by":"brian"}
  -> {"type":"proposed","timePeriod":4,"value":"AliceCo"}
  // proposal made using $LAV from Alice's promise as it included a lastAcceptedValue field

{"type":"promised","timePeriod":5,"by":"alice","lastAcceptedTimePeriod":1,"lastAcceptedValue":"AliceCo"}
  // nothing proposed - different $TIMEPERIOD from all earlier messages

{"type":"promised","timePeriod":5,"by":"brian","lastAcceptedTimePeriod":2,"lastAcceptedValue":"BrianCo"}
  -> {"type":"proposed","timePeriod":5,"value":"BrianCo"}
  // proposal made using $LAV from Brian's promise as it has the greater $LATP (so is fresher than Alice's)

{"type":"promised","timePeriod":6,"by":"brian","lastAcceptedTimePeriod":2,"lastAcceptedValue":"BrianCo"}
  // nothing proposed - different $TIMEPERIOD from all earlier messages

{"type":"promised","timePeriod":6,"by":"alice","lastAcceptedTimePeriod":1,"lastAcceptedValue":"AliceCo"}
  -> {"type":"proposed","timePeriod":6,"value":"brian's value"}
  // proposal made using $LAV from Brian's promise as it has the greater $LATP (so is fresher than Alice's)
```

### Acceptor

The Acceptor is the most complicated module. It has a name, `$NAME` (one of
`"alice"`, `"brian"` or `"chris"`) which will be agreed in advance as it must
not clash with that of the other Acceptors. It must include its name in the
`by` field of any messages it sends.

It receives two kinds of message:

```javascript
{"type":"prepare","timePeriod":$TIMEPERIOD}
{"type":"proposed","timePeriod":$TIMEPERIOD,"value":$VALUE}
```

The `prepare` message comes from the Nag and indicates the start of a new time
period. The `proposed` message comes from a Proposer and proposes a value for
the given time period.

Under the conditions set out below it may respond to these with:

```javascript
// in response to a 'prepare':
{"type":"promised","timePeriod":$TIMEPERIOD,"by":$NAME}
{"type":"promised","timePeriod":$TIMEPERIOD,"by":$NAME,"lastAcceptedTimePeriod":$LATP,"lastAcceptedValue":$LAV}

// in response to a 'proposed':
{"type":"accepted","timePeriod":$TIMEPERIOD,"by":$NAME,"value":$VALUE}
```

Throughout, `$TIMEPERIOD` and `$LATP` are positive integers, and all other
values are strings.

When it receives a `prepare` message the Acceptor should compare it to the last
`accepted` message it sent:

- If it has not yet sent any `accepted` messages, it should respond with a
  `promised` message without the `lastAccepted*` fields.

- If the last `accepted` message was sent in a _strictly_ earlier time period
  than the `prepare` message's `$TIMEPERIOD` then it should respond with a
`promised` message with the `lastAccepted*` fields, where `$LAV` and `$LATP`
are respectively set to the `$VALUE` and `$TIMEPERIOD` of the last `accepted`
message.

- If the last `accepted` message was sent in an equal or later time period than
  the `prepare` message's `$TIMEPERIOD` then no `promised` message is sent.

The purpose of the `promised` message is to indicate that this Acceptor will no
longer accept any proposals from earlier time periods.

When it receives a `proposed` message, it may respond with a corresponding
`accepted` message as long as

- doing so does not break any previous promises (i.e. it has not sent a
  `promised` message for a strictly later time period)
- doing so does not clash with any previous acceptances (i.e. all `accepted`
  messages sent so far were for strictly earlier time periods)

A simple Acceptor implementation is to track the greatest `$TIMEPERIOD` for
which an `accepted` message has been sent (along with the corresponding
`$VALUE`) and also to track the greatest `$TIMEPERIOD` for which a `promised`
message has been sent. Here is an example of the expected behaviour:

```javascript
{"type":"prepare","timePeriod":2}
  -> {"type":"promised","timePeriod":2,"by":"me"}
  // NB no "lastAccepted*" as nothing accepted yet

{"type":"prepare","timePeriod":1}
  -> {"type":"promised","timePeriod":1,"by":"me"}
  // ok to send out an earlier promise too

{"type":"prepare","timePeriod":2}
  -> {"type":"promised","timePeriod":2,"by":"me"}
  // ok to send out a duplicate promise

{"type":"proposed","timePeriod":1,"value":"value 1"}
  // have promised not to accept proposals from time periods before 2 so do not respond to this

{"type":"proposed","timePeriod":2,"value":"value 2"}
  -> {"type":"accepted","timePeriod":2,"by":"me","value":"value 2"}
  // ok to accept this proposal as it is consistent with all promises: it is not in a time period before 2

{"type":"prepare","timePeriod":1}
  // last accepted message was sent in a later time period, so no response

{"type":"prepare","timePeriod":2}
  // last accepted message was sent in this time period, so no response

{"type":"prepare","timePeriod":3}
  -> {"type":"promised","timePeriod":3,"by":"me","lastAcceptedTimePeriod":2,"lastAcceptedValue":"value 2"}
  // send out a promise, but now includes "lastAccepted*" fields

{"type":"proposed","timePeriod":4,"value":"value 4"}
  -> {"type":"accepted","timePeriod":4,"by":"me","value":"value 4"}
  // ok to accept this proposal as it is from time period >= 3 so consistent with all promises

{"type":"proposed","timePeriod":4,"value":"different value 4"}
  // have already accepted proposal 4, so do not accept it again.

{"type":"proposed","timePeriod":3,"value":"value 3"}
  // have already accepted proposal 4, so do not accept earlier-numbered
  // proposals.
```

### Comms details TODO

The modules will communicate via a central bus running in the cloud. Performing
a HTTP GET will return a JSON-formatted message, if one is available. If not,
the bus will wait for a while to see if one becomes available, and eventually
return `204 No Content` if the wait is fruitless. The module should then retry
its GET. Messages can be sent with a separate HTTP POST to the bus.

TODO Links to example code for interfacing with the bus.

### Protocol Flows

Here are some diagrams that illustrate how the modules described above all work
together to achieve consensus on a single value.

First consider the simplest situation: one proposer, one learner and three
acceptors (and a nag).

<img src='diagrams/001-setup.png' width='400px' />

When the nag's timeout expires, it broadcasts a `prepare` message to the
acceptors with `$PROP=1`.

<img src='diagrams/002-prepare.png' width='400px' />

In turn, the acceptors send `promised` messages to the proposer. Note that
since no acceptor has accepted a value yet, these `promised` messages do not
have a `max-accepted-value` field.

<img src='diagrams/003-promise.png' width='400px' />

When the proposer receives two of these `promised` messages, it broadcasts a
`proposed` message back to all the acceptors. Since none of the `promised`
messages have a `max-accepted-value` field, the value proposed is `$MYVALUE`.

<img src='diagrams/004-propose.png' width='400px' />

The acceptors may all accept this proposal as it's compatible with the promises
they made previously. They send `accepted` messages to the learner, and once
the learner receives two of these messages it learns the value and goes green.

<img src='diagrams/005-accept.png' width='400px' />

This demonstrates the basic message flow, but since there's only one proposer
there's only one value that could be proposed, and since there's only one
learner there is no risk of disagreeing on the value learned. Consider a more
complicated situation with two proposers and two learners as follows.

<img src='diagrams/010-setup.png' width='400px' />

Again, the nag broadcasts a `prepare` message with `$PROP=1`.

<img src='diagrams/011-prepare.png' width='400px' />

Again, the acceptors all send out `promised` messages. Note that this is not a
broadcast: these messages are routed to a single proposer. Here, since there
are two proposers this can be done by looking at whether `$PROP` is odd or
even: since it is odd, the promises are sent to the first proposer. Note also
that since no acceptor has accepted a value yet, these `promised` messages do
not have a `max-accepted-value` field.

<img src='diagrams/012-promise.png' width='400px' />

However, at this point a network glitch causes two of the `promised` messages
to be lost.

<img src='diagrams/013-promise-failed.png' width='400px' />

Since the proposer has only received a single `promised` message, it does
nothing. Eventually the nag broadcasts another `prepare` message, this time
with `$PROP=2`.

<img src='diagrams/014-retry-prepare.png' width='400px' />

The acceptors all send out further `promised` messages. Since `$PROP` is even,
they go to the second proposer.  Note that still no acceptor has accepted a
value yet, so these `promised` messages do not have a `max-accepted-value`
field.

<img src='diagrams/015-promise-2.png' width='400px' />

They arrive successfully, so the proposer broadcasts a `proposal` with its own
value.

<img src='diagrams/016-propose.png' width='400px' />

The acceptors may all accept this proposal as it's compatible with the promises
they made previously. They broadcast `accepted` messages to the learners, and
once each learner receives two of these messages it learns the value and goes
green.

<img src='diagrams/017-accept.png' width='400px' />

However, imagine that the network glitch earlier was only temporary and one
more `promised` messages is now delivered.

<img src='diagrams/018-delayed-promise.png' width='400px' />

At this point the first proposer broadcasts a `proposed` message with its own
value, which is different from the other proposer's value.

<img src='diagrams/019-delayed-propose.png' width='400px' />

But this `proposed` message is not compatible with the acceptors' promises any
more: they promised to accept no earlier proposition that `$PROP=2` but this
message has `$PROP=1`. The message is dropped, and there is no inconsistency in
the values accepted.

Here is an illustration of another kind of network glitch, showing the purpose
of the slightly mysterious `max-accepted-value` field. Again, it starts with
the nag broadcasting a `prepare` message with `$PROP=1`.

<img src='diagrams/020-prepare.png' width='400px' />

The accetors send out `promised` messages to the first proposer.

<img src='diagrams/021-promise.png' width='400px' />

When the proposer receives two, it proposes its value.

<img src='diagrams/022-propose.png' width='400px' />

This proposal is compatible with earlier promises, so it is accepted by all the
acceptors. But this time the network glitch drops all the messages to the
second learner. The first learner learns the proposed value and goes green but
the second is none the wiser.

<img src='diagrams/023-accept.png' width='400px' />

Eventually, the nag sends out another `prepare` message with `$PROP=2`.

<img src='diagrams/024-retry-prepare.png' width='400px' />

The acceptors send out `promised` messages in reaction. However, since they
have all previously accepted a value, these messages include the
`max-accepted-value` field containing the value they previously accepted.

<img src='diagrams/025-promise-2.png' width='400px' />

When the proposer receives two of these `promised` messages, it sends out a
proposal. Crucially, it may not propose its own value as it received a
`max-accepted-value`, which by the rules above it must propose instead. Thus it
proposes the same value as the other proposer previosuly proposed.

<img src='diagrams/026-propose-2.png' width='400px' />

The proposal is compatible with the promises, so it is accepted. This time, the
`accepted` messages make it through to the other learner so it now learns the
value.

<img src='diagrams/027-accept.png' width='400px' />

Thus the `max-accepted-value` field ensures that proposers cannot propose their
own values once a different value has been learned.

## Full Paxos

An extension, probably not possible in a single dojo. Note that deciding one
value is useful but really need to decide a _sequence_ of values.

Do this by running a sequence of Synod protocols as described above, called
_instances_ and numbered from 0. The instances are effectively independent
copies of the protocol running above, and messages are now decorated with an
`instance` field indicating the instance that they apply to.

### Learner

Again, the learner is the simplest module. The messages it now receives are the
same as before except for an extra `instance` field containing a number:

```javascript
{"instance":$INSTANCE,"type":"accepted","proposal":$PROP,"by":$NAME,"value":$VALUE}
```

It doesn't send any messages, but should report to the user when it has learned
a value. It learns a value by receiving two messages for the same `$PROP` (an
integer) and `$INSTANCE` but different `$NAME`s. In this case, the `$VALUE` of
the two messages will always be the same, so it desn't matter which one you
choose to report.

The previous implementation, keeping all received messages in a list, is still
a fine approach.



### Proposer

Again, the Proposer is the next simplest module. It now has a sequence of
constant strings, `$MYVALUE(0)`, `$MYVALUE(1)`,... where it would like all the
Learners to learn the string `$MYVALUE($INSTANCE)` for each instance. These
strings can be whatever you want as long as they do not change once the
protocol has started. It is a good idea to include your names or a team
identifier so you can track them. You can either define your sequence
statically or else allow the user to add to the sequence as the proposer runs.

As before, it receives messages that look like one of these (which are sent by
Acceptors):

```javascript
{"instance":$INSTANCE,"type":"promised","proposal":$PROP,"by":$NAME}
{"instance":$INSTANCE,"type":"promised","proposal":$PROP,"by":$NAME,"max-accepted-proposal":$MAXPROP,"max-accepted-value":$MAXVALUE}
```

In other words, the `max-accepted-proposal` and `max-accepted-value` fields are
optional, but not independently: either both are present or both are absent.

When it has received two of these `promised` messages for the same `$PROP` (an
integer) and `$INSTANCE` with different `$NAME`s, it should respond with a
message like this:

```javascript
{"instance":$INSTANCE,"type":"proposed","proposal":$PROP,"value":$VALUE}
```

The `$VALUE` should be calculated as described above.

There is now a third kind of message it might receive from an Acceptor:

```javascript
{"instance":$INSTANCE,"type":"promised","proposal":$PROP,"by":$NAME,"includes-greater-instances":true}
```

This should be handled as if it were a collection of messages like this, for
_every_ `$INSTANCE2 >= $INSTANCE`:

```javascript
{"instance":$INSTANCE2,"type":"promised","proposal":$PROP,"by":$NAME}
```

This means that, if you receive two of these `includes-greater-instances`
messages, you may respond with a whole collection of `proposed` messages: one
for each matching entry in your `$MYVALUE()` list.

The previous implementation, keeping all received messages in a list, is still
a fine approach.

### Acceptor

The Acceptor is again the most complicated module. It has a name, `$NAME` (one
of `"alice"`, `"brian"` or `"chris"`) which will be agreed in advance as it
must not clash with that of the other Acceptors. It should include this name in
the `by` field of any messages it sends.

It receives two kinds of message:

```javascript
{"instance":$INSTANCE,"type":"prepare","proposal":$PROP,"includes-greater-instance":true}
{"instance":$INSTANCE,"type":"proposed","proposal":$PROP,"value":$VALUE}
```

Similarly to before, it may respond to these with:

```javascript
// in response to a 'prepare':
{"instance":$INSTANCE,"type":"promised","proposal":$PROP,"by":$NAME}
{"instance":$INSTANCE,"type":"promised","proposal":$PROP,"by":$NAME,"max-accepted-proposal":$MAXPROP,"max-accepted-value":$MAXVALUE}
{"instance":$INSTANCE,"type":"promised","proposal":$PROP,"by":$NAME,"includes-greater-instances":true}

// in response to a 'proposed':
{"instance":$INSTANCE,"type":"accepted","proposal":$PROP,"by":$NAME,"value":$VALUE}
```

Note that the `prepare` message always includes greater instances, which
complicates the calculation of which `promised` messages (plural) to send.
It's always possible to send a promised message which includes greater
instances too, for a sufficiently large instance that no `accepted` message has
yet been sent. The remaining instances should be calculated separately as
before.

For an implementation, it remains viable to track all sent messages, or else
just the maximum accepted and promised values on an instance-by-instance basis.

## Further Enhancements

TODO more flexible majorities rather than just 2-of-3. Weight functions.

TODO only choose an instance when previous instance's value has been learned

TODO break down proposal ids - last digit is proposer id

TODO altering the topology. topology version in proposal id.

TODO stop working on an instance once value has been learned - discard associated data and ignore future messages. broadcast this. catch up.

TODO combine modules into a single thing
