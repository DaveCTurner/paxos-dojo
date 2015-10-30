using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Paxos.Messages
{
    public class Accepted
    {
        public string Type { get { return "accepted"; } }
        public int TimePeriod { get; set; }
        public string By { get; set; }
        public string Value { get; set; }
    }
}