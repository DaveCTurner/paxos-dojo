#!/usr/bin/ruby

require_relative 'messenger'

messenger = Messenger.new('http://127.0.0.1:24192/l/dt-rb')
received = []

while true do

  currMessage = messenger.getMessage

  for prevMessage in received do
    if currMessage['timePeriod'] != prevMessage['timePeriod'] then next end
    if currMessage['by'] == prevMessage['by'] then next end

    print "Value learned: '" + currMessage.to_s + "'\n"
    break
  end

  received << currMessage

end
