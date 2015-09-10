describe("Learner", function() {

  var previousMessages = [];

  [ { expectLearned:null, when:"only one message is received"
    , message:{"type":"accepted","timePeriod":1,"by":"alice","value":"value 1"}}
  , { expectLearned:null, when:"a message received from a different time period"
    , message:{"type":"accepted","timePeriod":2,"by":"brian","value":"value 1"}}
  , { expectLearned:null, when:"a duplicate message received"
    , message:{"type":"accepted","timePeriod":2,"by":"brian","value":"value 1"}}
  , { expectLearned:"value 1", when:"acceptances received from a majority"
    , message:{"type":"accepted","timePeriod":2,"by":"chris","value":"value 1"}}
  ].forEach(function(testCase) {

    var learnedValue = null;
    var learner = makeLearner(function(value) { learnedValue = value; });

    previousMessages.push(testCase.message);
    var currentMessages = previousMessages.slice();

    it('expects to have learned ' + testCase.expectLearned + ' when ' + testCase.when, function() {
      currentMessages.forEach(learner);
      expect(learnedValue).toBe(testCase.expectLearned);
    });

  });
});

describe("Proposer", function() {

  var receivedMessages = [];

  [ {   onReceiptOf: {"type":"promised","timePeriod":1,"by":"alice"},
        expectToSend: [],
        because: "this is the first promise"

  }, {  onReceiptOf: {"type":"promised","timePeriod":2,"by":"brian"},
        expectToSend: [],
        because: "this is for a different timePeriod than the previous message"

  }, {  onReceiptOf: {"type":"promised","timePeriod":2,"by":"brian"},
        expectToSend: [],
        because: "this is a duplicate of the previous message",

  }, {  onReceiptOf: {"type":"promised","timePeriod":2,"by":"chris"},
        expectToSend: [{"type":"proposed","timePeriod":2,"value":"value from Jasmine"}],
        because: "have now received promises from a majority of Acceptors",

  }, {  onReceiptOf: {"type":"promised","timePeriod":2,"by":"alice"},
        expectToSend: [],
        because: "have already made a proposal in time period 2",

  }, {  onReceiptOf: {"type":"promised","timePeriod":3,"by":"brian"},
        expectToSend: [],
        because: "this is the first message in time period 3",

  }, {  onReceiptOf: {"type":"promised","timePeriod":3,"by":"alice","lastAcceptedTimePeriod":1,"lastAcceptedValue":"AliceCo"},
        expectToSend: [{"type":"proposed","timePeriod":3,"value":"AliceCo"}],
        because: "have received promises from a majority, but Alice's included a lastAcceptedValue field"

  }, {  onReceiptOf: {"type":"promised","timePeriod":4,"by":"alice","lastAcceptedTimePeriod":1,"lastAcceptedValue":"AliceCo"},
        expectToSend: [],
        because: "this is the first message in time period 4"

  }, {  onReceiptOf: {"type":"promised","timePeriod":4,"by":"brian"},
        expectToSend: [{"type":"proposed","timePeriod":4,"value":"AliceCo"}],
        because: "have received promises from a majority, and Alice's included a lastAcceptedValue field"

  }, {  onReceiptOf: {"type":"promised","timePeriod":5,"by":"alice","lastAcceptedTimePeriod":1,"lastAcceptedValue":"AliceCo"},
        expectToSend: [],
        because: "this is the first message in time period 5"

  }, {  onReceiptOf: {"type":"promised","timePeriod":5,"by":"brian","lastAcceptedTimePeriod":2,"lastAcceptedValue":"BrianCo"},
        expectToSend: [{"type":"proposed","timePeriod":5,"value":"BrianCo"}],
        because: "have received promises from a majority, both with lastAcceptedValue field, but Brian's is from a later period"

  }, {  onReceiptOf: {"type":"promised","timePeriod":6,"by":"brian","lastAcceptedTimePeriod":2,"lastAcceptedValue":"BrianCo"},
        expectToSend: [],
        because: "this is the first message in time period 6"

  }, {  onReceiptOf: {"type":"promised","timePeriod":6,"by":"alice","lastAcceptedTimePeriod":1,"lastAcceptedValue":"AliceCo"},
        expectToSend: [{"type":"proposed","timePeriod":6,"value":"BrianCo"}],
        because: "have received promises from a majority, both with lastAcceptedValue field, but Brian's is from a later period"

  }].forEach(function(testCase) {

    var receivedPreviously = receivedMessages.slice();

    it('expects to send ' + JSON.stringify(testCase.expectToSend)
      + ' on receipt of ' + JSON.stringify(testCase.onReceiptOf)
      + ' because ' + testCase.because,

      function() {
        var proposer = makeProposer('value from Jasmine');
        var actuallySent = [];
        receivedPreviously.forEach(function(message) { proposer(message, function() { })});
        proposer(testCase.onReceiptOf, function(response) { actuallySent.push(response); });
        expect(actuallySent).toEqual(testCase.expectToSend);
      });

    receivedMessages.push(testCase.onReceiptOf);

  });
});

describe("Acceptor", function() {

  var receivedMessages = [];

  [ {   onReceiptOf: {"type":"prepare","timePeriod":2},
        expectToSend: [{"type":"promised","timePeriod":2,"by":"me","haveAccepted":false}],
        because: "can promise anything as nothing has been accepted yet"

  }, {  onReceiptOf: {"type":"prepare","timePeriod":1},
        expectToSend: [{"type":"promised","timePeriod":1,"by":"me","haveAccepted":false}],
        because: "it is ok to send out a promise for an earlier time period too"

  }, {  onReceiptOf: {"type":"prepare","timePeriod":2},
        expectToSend: [{"type":"promised","timePeriod":2,"by":"me","haveAccepted":false}],
        because: "it is ok to send out a duplicate promise"

  }, {  onReceiptOf: {"type":"proposed","timePeriod":1,"value":"value 1"},
        expectToSend: [],
        because: "accepting this would break the previous promise"

  }, {  onReceiptOf: {"type":"proposed","timePeriod":2,"value":"value 2"},
        expectToSend: [{"type":"accepted","timePeriod":2,"by":"me","value":"value 2"}],
        because: "this is consistent with all promises as it is for a time period >= 2"

  }, {  onReceiptOf: {"type":"prepare","timePeriod":1},
        expectToSend: [],
        because: "have accepted a value for a later time period"

  }, {  onReceiptOf: {"type":"prepare","timePeriod":2},
        expectToSend: [],
        because: "have accepted a value for this time period"

  }, {  onReceiptOf: {"type":"prepare","timePeriod":3},
        expectToSend: [{"type":"promised","timePeriod":3,"by":"me","lastAcceptedTimePeriod":2,"lastAcceptedValue":"value 2"}],
        because: "ok to promise, but must now include lastAccepted* fields"

  }, {  onReceiptOf: {"type":"proposed","timePeriod":4,"value":"value 4"},
        expectToSend: [{"type":"accepted","timePeriod":4,"by":"me","value":"value 4"}],
        because: "it is consistent with all promises as for a time period >= 3"

  }, {  onReceiptOf: {"type":"proposed","timePeriod":4,"value":"different value 4"},
        expectToSend: [],
        because: "have already accepted a proposal in time period 4, so do not accept it again"

  }, {  onReceiptOf: {"type":"proposed","timePeriod":3,"value":"value 3"},
        expectToSend: [],
        because: "have already accepted a proposal in time period 4, so do not accept earlier proposals"

  }].forEach(function(testCase) {

    var receivedPreviously = receivedMessages.slice();

    it('expects to send ' + JSON.stringify(testCase.expectToSend)
      + ' on receipt of ' + JSON.stringify(testCase.onReceiptOf)
      + ' because ' + testCase.because,

      function() {
        var acceptor = makeAcceptor('me');
        var actuallySent = [];
        receivedPreviously.forEach(function(message) { acceptor(message, function() { })});
        acceptor(testCase.onReceiptOf, function(response) { actuallySent.push(response); });
        expect(actuallySent).toEqual(testCase.expectToSend);
      });

    receivedMessages.push(testCase.onReceiptOf);
  });
});
