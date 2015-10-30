using System;
using System.Collections.Generic;
using System.Linq;
using Paxos.Messages;

namespace Paxos
{
    static class Program
    {
        static void Main()
        {
            System.Net.ServicePointManager.DefaultConnectionLimit = 20;
            RunNode("http://paxos.leedscodedojo.org.uk/dev/l/csharp");
        }

        private static void RunNode(string uri)
        {
            var messenger = new Messenger(uri);

            while (true)
            {
                // Receive messages like this
                var currMessage = messenger.GetMessage<Accepted>();

                Console.WriteLine("Received 'accepted' message by {0} for value '{1}' in time period {2}", currMessage.By, currMessage.Value, currMessage.TimePeriod);

                // Send messages back like this
                messenger.PostMessage(new Promised
                {
                    By = "foo",
                    TimePeriod = currMessage.TimePeriod,
                });
            }
        }
    }
}
