class Message
  attr_accessor :timePeriod, :by, :type, :haveAccepted, :lastAcceptedTimePeriod, :lastAcceptedValue

  def initialize(timePeriod, by, type, haveAccepted, lastAcceptedTimePeriod = nil, lastAcceptedValue = nil)
    @timePeriod = timePeriod
    @by = by
    @type = type
    @haveAccepted = haveAccepted
    @lastAcceptedTimePeriod = lastAcceptedTimePeriod
    @lastAcceptedValue = lastAcceptedValue
  end
end
