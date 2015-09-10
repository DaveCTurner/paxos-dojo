#!/usr/bin/ruby

require_relative 'messenger'

name = 'alice'
messenger = Messenger.new('http://127.0.0.1:24192/a/dt-rb')

latestProposedTimePeriod = 0
promisedToAcceptNoEarlierThan = 0
lastAccepted = -1
lastAcceptedValue = ''

while true do

  currMessage = messenger.getMessage

  if currMessage['timePeriod'] <= lastAccepted then next end
  response = {'by' => name, 'timePeriod' => currMessage['timePeriod']}

  if currMessage['type'] == 'prepare' then
    if currMessage['timePeriod'] > promisedToAcceptNoEarlierThan then
      promisedToAcceptNoEarlierThan = currMessage['timePeriod']
    end

    response['type'] = 'promised'

    if lastAccepted >= 0 then
      response['lastAcceptedTimePeriod'] = lastAccepted
      response['lastAcceptedValue']      = lastAcceptedValue
    else
      response['haveAccepted']           = false
    end

    messenger.sendMessage(response)

  elsif currMessage['type'] == 'proposed' then
    if currMessage['timePeriod'] < promisedToAcceptNoEarlierThan then next end

    lastAccepted = currMessage['timePeriod']
    lastAcceptedValue = currMessage['value']

    response['type'] = 'accepted'
    response['value'] = currMessage['value']

    messenger.sendMessage(response)

  end

end

