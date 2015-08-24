import urllib2
import httplib
import json
import time

class Messenger:
  """Class for communicating with Paxos cluster"""
  def __init__(self, url):
    self.url = url

  def getNextMessage(self):
    while True:
      try:
        response = urllib2.urlopen(self.url)
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
      except httplib.BadStatusLine as e:
        print e
        time.sleep(2.0)

  def postMessage(self, message):
    req = urllib2.Request(self.url)
    req.add_header('Content-Type', 'application/json')
    try:
      urllib2.urlopen(req, json.dumps(message))
    except urllib2.URLError as e:
      print e

