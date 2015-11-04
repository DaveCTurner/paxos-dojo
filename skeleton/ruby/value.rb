class Value
  attr_accessor :all_msg, :new_msg

  def initialize(all_msg,new_msg)
    @all_msg = all_msg
    @new_msg = new_msg
  end

  def getValue
    if same_time_diff_name?
      if message_has_lav?
        return message_lav.lastAcceptedValue
      end
      return 'Rubys Awesome Codez'
    end
  end

  private
  def same_time_diff_name
    all_msg.select { |message| 
      message.by != new_msg.by 
    }.select { |message| 
      message.timePeriod == new_msg.timePeriod
    }.first
  end

  def same_time_diff_name?
    !same_time_diff_name.nil?
  end

  def message_has_lav?
    same_time_diff_name.lastAcceptedValue || new_msg.lastAcceptedValue
  end

  def message_lav 
    [new_msg, same_time_diff_name]
      .select { |message| message.lastAcceptedTimePeriod}
      .sort{|x,y| x.lastAcceptedTimePeriod <=> y.lastAcceptedTimePeriod }
      .last
  end
end
