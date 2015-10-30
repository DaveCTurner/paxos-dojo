using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace Paxos
{
    public class Messenger
    {
        private readonly string _uri;

        public Messenger(string uri)
        {
            _uri = uri;
        }

        public T GetMessage<T>()
        {
            while (true)
            {
                try
                {
                    var request = (HttpWebRequest) WebRequest.Create(_uri);
                    using (var response = (HttpWebResponse) request.GetResponse())
                    {
                        if (response.StatusCode == HttpStatusCode.NoContent)
                            continue;

                        using (var reader = new StreamReader(response.GetResponseStream()))
                        {
                            var responseBody = reader.ReadToEnd();
                            return JsonConvert.DeserializeObject<T>(responseBody);
                        }
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine("Exception in GetMessage(): {0}", e);
                    Thread.Sleep(TimeSpan.FromSeconds(2));
                }
            }
        }

        public void PostMessage<T>(T message)
        {
            try
            {
                var request = (HttpWebRequest)WebRequest.Create(_uri);
                request.ContentType = "application/json";
                request.Method = "POST";

                var settings = new JsonSerializerSettings
                {
                    Formatting = Formatting.Indented,
                    ContractResolver = new CamelCasePropertyNamesContractResolver()
                };

                using (var writer = new StreamWriter(request.GetRequestStream()))
                    writer.Write(JsonConvert.SerializeObject(message, typeof(T), settings));

                using (var response = (HttpWebResponse) request.GetResponse())
                {
                    if (response.StatusCode != HttpStatusCode.NoContent)
                        Console.WriteLine("PostMessage(): got {0}", response.StatusCode);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Exception in PostMessage(): {0}", e);
            }
        }
    }
}