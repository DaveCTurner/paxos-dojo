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
    begin
      http = Net::HTTP.new(@url.host, @url.port)
      req = Net::HTTP::Post.new(@url.path, initheader = {'Content-type' => 'application/json'})
      req.body = message.to_json
      response = http.request(req)
      if response.code != '200' then
        print "ERROR: sendMessage returned " + response.code + "\n"
      end
    rescue Errno::ECONNREFUSED
    rescue EOFError
    end
  end
end
