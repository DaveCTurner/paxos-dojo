#!/usr/bin/python

from messenger import Messenger

messenger = Messenger('http://127.0.0.1:24192/proposer/dt-py')
received = []
latestProposedTimePeriod = 0;

while True:
  currMessage = messenger.getNextMessage()
  if currMessage['timePeriod'] <= latestProposedTimePeriod: continue
  for prevMessage in received:
    if currMessage['timePeriod'] != prevMessage['timePeriod']: continue
    if currMessage['by'] == prevMessage['by']: continue

    proposedValue = myValue
    if currMessage['lastAcceptedTimePeriod']:
      proposedValue = currMessage['lastAcceptedTimePeriod']

  received.append(currMessage)
