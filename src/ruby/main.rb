#!/usr/bin/ruby

require 'net/http'
require 'json'

class Messenger
  def initialize(url)
    @url = URI(url)
  end

  def getMessage
    while true do

      begin
        response = Net::HTTP.get_response(@url)
        if response.code == '200' then
          return JSON.parse(response.body)
        end
      rescue JSON::ParserError
        sleep 2
      rescue Errno::ECONNREFUSED
        sleep 2
      rescue EOFError
        sleep 2
      end

    end
  end

  def sendMessage(message)
    print "Sending '" + message.to_s + "\n"
    begin
      http = Net::HTTP.new(@url.host, @url.port)
      req = Net::HTTP::Post.new(@url.path, initheader = {'Content-type' => 'application/json'})
      req.body = message.to_json
      http.request(req)
    rescue Errno::ECONNREFUSED
    rescue EOFError
    end
  end
end

class Learner
  def initialize
    @received = []
  end

  def handleMessage(currMessage)
    if currMessage['type'] == 'accepted' then
      for prevMessage in @received do
        if currMessage['timePeriod'] != prevMessage['timePeriod'] then next end
        if currMessage['by'] == prevMessage['by'] then next end

        print "Value learned: '" + currMessage.to_s + "'\n"
        break
      end
      @received << currMessage
    end
  end
end

class Proposer
  def initialize(messenger, myValue)
    @messenger = messenger
    @myValue = myValue
    @received = []
    @latestProposedTimePeriod = 0
  end

  def handleMessage(currMessage)
    if currMessage['type'] == 'promised' then
      if currMessage['timePeriod'] <= @latestProposedTimePeriod then return end

      for prevMessage in @received do
        if currMessage['timePeriod'] != prevMessage['timePeriod'] then next end
        if currMessage['by'] == prevMessage['by'] then next end

        proposedValue = @myValue

        if currMessage['lastAcceptedTimePeriod'] then
          proposedValue = currMessage['lastAcceptedValue']
        end

        if prevMessage['lastAcceptedTimePeriod'] and \
              (!currMessage['lastAcceptedTimePeriod'] \
              or prevMessage['lastAcceptedTimePeriod'] > currMessage['lastAcceptedTimePeriod']) then
          proposedValue = prevMessage['lastAcceptedValue']
        end

        @messenger.sendMessage(                     \
          {'type'=>'proposed'                       \
          ,'timePeriod'=>currMessage['timePeriod']  \
          ,'value'=>proposedValue})

        @latestProposedTimePeriod = currMessage['timePeriod']

        break
      end
      @received << currMessage
    end
  end
end

class Acceptor
  def initialize(messenger, name)
    @messenger = messenger
    @name = name
    @promisedToAcceptNoEarlierThan = 0
    @lastAccepted = -1
    @lastAcceptedValue = ''
  end

  def handleMessage(currMessage)
    if currMessage['timePeriod'] <= @lastAccepted then return end
    response = {'by' => @name, 'timePeriod' => currMessage['timePeriod']}

    if currMessage['type'] == 'prepare' then
      if currMessage['timePeriod'] > @promisedToAcceptNoEarlierThan then
        @promisedToAcceptNoEarlierThan = currMessage['timePeriod']
      end

      response['type'] = 'promised'

      if @lastAccepted >= 0 then
        response['lastAcceptedTimePeriod'] = @lastAccepted
        response['lastAcceptedValue']      = @lastAcceptedValue
      end

      @messenger.sendMessage(response)

    elsif currMessage['type'] == 'proposed' then
      if currMessage['timePeriod'] < @promisedToAcceptNoEarlierThan then return end
  
      @lastAccepted = currMessage['timePeriod']
      @lastAcceptedValue = currMessage['value']

      response['type'] = 'accepted'
      response['value'] = currMessage['value']

      @messenger.sendMessage(response)

    end
  end
end
    

messenger = Messenger.new('http://127.0.0.1:24192/general/dt-rb')
learner   = Learner.new
proposer  = Proposer.new(Messenger.new('http://127.0.0.1:24192/proposer/dt-rb'), 'value from ruby')
acceptor  = Acceptor.new(Messenger.new('http://127.0.0.1:24192/acceptor/dt-rb'), 'alice')

while true do

  currMessage = messenger.getMessage
  learner.handleMessage(currMessage)
  proposer.handleMessage(currMessage)
  acceptor.handleMessage(currMessage)

end

