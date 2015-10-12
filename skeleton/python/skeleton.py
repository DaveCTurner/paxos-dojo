#!/usr/bin/python

from messenger import Messenger
from os        import getpid

messenger = Messenger('http://leedscodedojo.tessanddave.com/paxos-test/a/xx-' + str(getpid()).zfill(5))

while True:
  currMessage = messenger.getNextMessage()
  print "Received: " + str(currMessage)

  ''' process currMessage as described, possibly sending back some other messages, e.g.:

  messenger.postMessage({'type':'messageType','timePeriod':timePeriod})
  '''

