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
