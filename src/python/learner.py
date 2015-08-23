#!/usr/bin/python

from messenger import Messenger

messenger = Messenger('http://127.0.0.1:24192/learner/5')
received = []

while True:
  currMessage = messenger.getNextMessage()
  for prevMessage in received:
    if currMessage['timePeriod'] == prevMessage['timePeriod'] and currMessage['by'] != prevMessage['by']:
      print "Value '" + currMessage['value'] + "' learned"
      break
  received.append(currMessage)
