namespace Paxos.Messages
{
    public class Proposed
    {
        public string Type { get { return "proposed"; } }
        public int TimePeriod { get; set; }
        public string Value { get; set; }
    }
}