#!/usr/bin/python

from messenger import Messenger

messenger = Messenger('http://127.0.0.1:24192/learner/5');

proposal = 0;

while True:
  currMessage = messenger.getNextMessage()
  print "Received: " + str(currMessage)

  print "value: " + currMessage['value']

  ''' process currMessage as described, possibly sending back some other messages, e.g.:

  messenger.postMessage({'type':'messageType','timePeriod':timePeriod})
  '''

