namespace Paxos.Messages
{
    public class Promised
    {
        public Promised()
        {
            HaveAccepted = true;
        }

        public string Type { get { return "promised"; } }
        public int TimePeriod { get; set; }
        public string By { get; set; }
        public bool HaveAccepted { get; set; }
        public int LastAcceptedTimePeriod { get; set; }
        public string LastAcceptedValue { get; set; }
    }
}