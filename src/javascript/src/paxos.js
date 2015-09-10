
function makeLearner(onLearned) {
  var learnerReceived = [];

  return function(currMessage) {
    var haveLearned = false;
    learnerReceived.forEach(function(prevMessage) {
      if (!haveLearned && currMessage.timePeriod === prevMessage.timePeriod && currMessage.by != prevMessage.by) {
        haveLearned = true;
        if (onLearned) { onLearned(currMessage.value); }
      }
    });
    learnerReceived.push(currMessage);
  };
}

function makeProposer(myValue) {
  var proposerReceived = [];
  var latestProposedTimePeriod = 0;

  return function(currMessage, respond) {
    if (currMessage.timePeriod <= latestProposedTimePeriod) { return; }
    var haveProposed = false;
    proposerReceived.forEach(function(prevMessage) {
      if (haveProposed) { return; }
      if (currMessage.timePeriod != prevMessage.timePeriod) { return; }
      if (currMessage.by == prevMessage.by) { return; }
      var proposedValue = myValue;

      if (currMessage.lastAcceptedTimePeriod) {
        proposedValue = currMessage.lastAcceptedValue;
      }

      if (prevMessage.lastAcceptedTimePeriod
            && (!currMessage.lastAcceptedTimePeriod
                || prevMessage.lastAcceptedTimePeriod > currMessage.lastAcceptedTimePeriod)) {
        proposedValue = prevMessage.lastAcceptedValue;
      }
      respond({type:'proposed',timePeriod:currMessage.timePeriod,value:proposedValue});
      latestProposedTimePeriod = currMessage.timePeriod;
      haveProposed = true;
    });
    proposerReceived.push(currMessage);
  };
}

function makeAcceptor(name) {
  var promisedToAcceptNoEarlierThan = 0;
  var lastAccepted = -1;
  var lastAcceptedValue = '';

  return function(currMessage, respond) {
    var response = { by: name, timePeriod: currMessage.timePeriod };
    if (currMessage.type === 'prepare') {
      if (currMessage.timePeriod <= lastAccepted) { return; }
      if (currMessage.timePeriod > promisedToAcceptNoEarlierThan) { promisedToAcceptNoEarlierThan = currMessage.timePeriod; }

      response.type = 'promised';

      if (lastAccepted >= 0) {
        response.lastAcceptedTimePeriod = lastAccepted;
        response.lastAcceptedValue      = lastAcceptedValue;
      } else {
        response.haveAccepted = false;
      }
      respond(response);
    
    } else if (currMessage.type === 'proposed') {
      if (currMessage.timePeriod < promisedToAcceptNoEarlierThan) { return; }
      if (currMessage.timePeriod <= lastAccepted) { return; }

      lastAccepted = currMessage.timePeriod;
      lastAcceptedValue = currMessage.value;
      response.type = 'accepted';
      response.value = currMessage.value;
      respond(response);
    }
  };
};
