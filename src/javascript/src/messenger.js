function runModule(url, cb) {
  var respond = function(message) {
    var req = new XMLHttpRequest();
    req.open('POST', url);
    req.setRequestHeader('Content-type', 'application/json');
    req.send(JSON.stringify(message));
  }

  var req = new XMLHttpRequest();
  req.onreadystatechange = function() {
    if (req.readyState != 4) {
      return;
    }

    var isHttpSuccess = 200 <= req.status && req.status <= 299;
    setTimeout(function() { runModule(url, cb) }, isHttpSuccess ? 0 : 2000);

    if (req.status == 200) {
      cb(JSON.parse(req.responseText), respond);
    }
  }
  req.open('GET', url);
  req.send();
}
