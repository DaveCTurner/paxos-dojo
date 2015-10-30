using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Paxos.Messages
{
    public class AcceptorReceived
    {
        public string Type { get; set; }
        public int TimePeriod { get; set; }
        public string Value { get; set; }
    }
}