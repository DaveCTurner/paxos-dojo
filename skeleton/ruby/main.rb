#!/usr/bin/ruby

require_relative 'messenger'

name = 'alice'
messenger = Messenger.new(('http://leedscodedojo.tessanddave.com/paxos-test/a/xx-%05d-' % Process.pid()) + name)

while true do

  currMessage = messenger.getMessage
  print ("Received:   " + currMessage.to_s() + "\n")

  response = {'by' => name, 'timePeriod' => currMessage['timePeriod']}
  print ("Responding: " + response.to_s() + "\n")

  messenger.sendMessage(response)

end

