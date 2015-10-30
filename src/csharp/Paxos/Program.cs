using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Paxos.Messages;

namespace Paxos
{
    static class Program
    {
        static void Main()
        {
            System.Net.ServicePointManager.DefaultConnectionLimit = 20;

            new Thread(() => RunProposer("http://paxos.leedscodedojo.org.uk/live/p/dt-cs", "My value")).Start();
            new Thread(() => RunLearner("http://paxos.leedscodedojo.org.uk/live/l/dt-cs")).Start();
            new Thread(() => RunAcceptor("http://paxos.leedscodedojo.org.uk/live/a/dt-cs-1", "alice")).Start();
            new Thread(() => RunAcceptor("http://paxos.leedscodedojo.org.uk/live/a/dt-cs-2", "brian")).Start();
        }

        private static void RunAcceptor(string uri, string acceptorName)
        {
            var messenger = new Messenger(uri);
            var promisedToAcceptNoEarlierThan = 0;
            var lastAccepted = -1;
            var lastAcceptedValue = "";

            while (true)
            {
                var currMessage = messenger.GetMessage<AcceptorReceived>();

                if (currMessage.TimePeriod <= lastAccepted) continue;

                switch (currMessage.Type)
                {
                    case "prepare":
                        if (currMessage.TimePeriod > promisedToAcceptNoEarlierThan)
                            promisedToAcceptNoEarlierThan = currMessage.TimePeriod;

                        messenger.PostMessage(new Promised
                        {
                            By = acceptorName,
                            TimePeriod = currMessage.TimePeriod,
                            HaveAccepted = lastAccepted > 0,
                            LastAcceptedTimePeriod = lastAccepted,
                            LastAcceptedValue = lastAcceptedValue
                        });

                        break;

                    case "proposed":

                        if (currMessage.TimePeriod < promisedToAcceptNoEarlierThan)
                            continue;

                        lastAccepted = currMessage.TimePeriod;
                        lastAcceptedValue = currMessage.Value;

                        messenger.PostMessage(new Accepted
                        {
                            By = acceptorName,
                            TimePeriod = currMessage.TimePeriod,
                            Value = currMessage.Value,
                        });

                        break;
                }
            }
        }

        private static void RunLearner(string uri)
        {
            var messenger = new Messenger(uri);
            var received = new List<Accepted>();

            while (true)
            {
                var currMessage = messenger.GetMessage<Accepted>();

                var matchingMessage = received.FirstOrDefault(
                    prevMessage => currMessage.TimePeriod == prevMessage.TimePeriod && currMessage.By != prevMessage.By);

                received.Add(currMessage);

                if (matchingMessage == null) continue;

                Console.WriteLine("Learned value '{0}' in time period {1}", currMessage.Value, currMessage.TimePeriod);
            }
        }

        private static void RunProposer(string uri, string freeValue)
        {
            var messenger = new Messenger(uri);
            var received = new List<Promised>();
            var latestProposedTimePeriod = 0;

            while (true)
            {
                var currMessage = messenger.GetMessage<Promised>();
                if (currMessage.TimePeriod <= latestProposedTimePeriod) continue;

                var matchingMessage =
                    received.FirstOrDefault(
                        prevMessage =>
                            currMessage.TimePeriod == prevMessage.TimePeriod && currMessage.By != prevMessage.By);

                received.Add(currMessage);

                if (matchingMessage == null) continue;

                var value = freeValue;

                if (currMessage.HaveAccepted)
                {
                    value = currMessage.LastAcceptedValue;
                }

                if (matchingMessage.HaveAccepted && (!currMessage.HaveAccepted
                                                     ||
                                                     matchingMessage.LastAcceptedTimePeriod >
                                                     currMessage.LastAcceptedTimePeriod))
                    value = matchingMessage.LastAcceptedValue;

                messenger.PostMessage(new Proposed
                {
                    TimePeriod = currMessage.TimePeriod,
                    Value = value,
                });

                Console.WriteLine("Proposed value '{0}' in time period {1}", value, currMessage.TimePeriod);

                latestProposedTimePeriod = currMessage.TimePeriod;
            }
        }
    }
}
