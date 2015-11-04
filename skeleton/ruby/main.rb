#!/usr/bin/ruby

require_relative 'messenger'
require_relative 'message'
require_relative 'value'

name = 'proposed'
messenger = Messenger.new(('http://paxos.leedscodedojo.org.uk/dev/p/ab' % Process.pid()) + name)
all_msg, latestProposedTime = [], 0

while true do

  currMessage = messenger.getMessage
  print ("Received:   " + currMessage.to_s() + "\n")

  new_msg = Message.new(
                        currMessage['timePeriod'], currMessage['by'], currMessage['by'],
                        currMessage['haveAccepted'],currMessage['lastAcceptedTimePeriod'],
                        currMessage['lastAcceptedValue']
                        )

  if(new_msg.timePeriod > latestProposedTime)
    value = Value.new(all_msg, new_msg).getValue
    if (value)
      latestProposedTime = new_msg.timePeriod
      response = {'type' => name, 'timePeriod' => currMessage['timePeriod'], 'value' => value}
      print ("Responding: " + response.to_s() + "\n")
      messenger.sendMessage(response)
    end
    all_msg << new_msg
  end
end
