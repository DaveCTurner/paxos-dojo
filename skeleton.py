#!/usr/bin/python

import urllib2
import json
import time

url = 'http://127.0.0.1:24192/learner/5'

def getNextMessage():
  while True:
    try:
      response = urllib2.urlopen(url)
      try:
        if response.code == 200:
          return json.load(response)
      finally:
        response.close()
    except ValueError as e:
      print e
      time.sleep(2.0)
    except urllib2.URLError as e:
      print e
      time.sleep(2.0)

def postMessage(message):
  req = urllib2.Request(url)
  req.add_header('Content-Type', 'application/json')
  try:
    urllib2.urlopen(req, json.dumps(message))
  except urllib2.URLError as e:
    print e

while True:
  currMessage = getNextMessage()
  print "Received: " + str(currMessage)

  ''' process currMessage as described, possibly sending back some other messages, e.g.:

  print "Proposing", proposal
  postMessage({'type':'prepare','proposal':proposal})
  proposal += 1
  '''

