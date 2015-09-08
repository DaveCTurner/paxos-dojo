#!/usr/bin/python

from messenger import Messenger
from os        import getpid
from sys       import argv

acceptorName = argv[1]
messenger = Messenger('http://127.0.0.1:24192/a/dt-py-' + str(getpid()).zfill(5) + '-' + acceptorName)
promisedToAcceptNoEarlierThan = 0
lastAccepted = -1

while True:
  currMessage = messenger.getNextMessage()
  if currMessage['timePeriod'] <= lastAccepted: continue
  response = {'by':acceptorName, 'timePeriod':currMessage['timePeriod']}

  if currMessage['type'] == 'prepare':
    if currMessage['timePeriod'] > promisedToAcceptNoEarlierThan:
      promisedToAcceptNoEarlierThan = currMessage['timePeriod']
    
    response['type'] = 'promised'

    if lastAccepted >= 0:
      response['lastAcceptedTimePeriod'] = lastAccepted
      response['lastAcceptedValue']      = lastAcceptedValue
    else:
      response['haveAccepted']           = False

  elif currMessage['type'] == 'proposed':
    if currMessage['timePeriod'] < promisedToAcceptNoEarlierThan: continue

    lastAccepted = currMessage['timePeriod']
    lastAcceptedValue = currMessage['value']

    response['type'] = 'accepted'
    response['value'] = currMessage['value']

  messenger.postMessage(response)
