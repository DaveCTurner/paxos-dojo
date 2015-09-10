#!/usr/bin/ruby

require_relative 'messenger'

freeValue = 'value from ruby 2'
messenger = Messenger.new('http://127.0.0.1:24192/p/dt-rb')

received = []
latestProposedTimePeriod = 0

while true do

  currMessage = messenger.getMessage
  if currMessage['timePeriod'] <= latestProposedTimePeriod then next end

  for prevMessage in received do
    if currMessage['timePeriod'] != prevMessage['timePeriod'] then next end
    if currMessage['by'] == prevMessage['by'] then next end

    proposedValue = freeValue

    if currMessage['lastAcceptedTimePeriod'] then
      proposedValue = currMessage['lastAcceptedValue']
    end

    if prevMessage['lastAcceptedTimePeriod'] and \
          (!currMessage['lastAcceptedTimePeriod'] \
          or prevMessage['lastAcceptedTimePeriod'] > currMessage['lastAcceptedTimePeriod']) then
      proposedValue = prevMessage['lastAcceptedValue']
    end

    messenger.sendMessage(                     \
      {'type'=>'proposed'                       \
      ,'timePeriod'=>currMessage['timePeriod']  \
      ,'value'=>proposedValue})

    latestProposedTimePeriod = currMessage['timePeriod']

    break
  end

  received << currMessage

end

