#!/usr/bin/python

from messenger import Messenger
from os        import getpid

messenger = Messenger('http://127.0.0.1:24192/l/dt-py-' + str(getpid()).zfill(5))
received = []

while True:
  currMessage = messenger.getNextMessage()
  for prevMessage in received:
    if currMessage['timePeriod'] == prevMessage['timePeriod'] and currMessage['by'] != prevMessage['by']:
      print "Value '" + currMessage['value'] + "' learned"
      break
  received.append(currMessage)
