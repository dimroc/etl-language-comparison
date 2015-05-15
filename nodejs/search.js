var fs         = require('fs');
var readline   = require('readline');
var files      = fs.readdirSync('./tmp/tweets');
var cluster    = require('cluster');
var numWorkers = require('os').cpus().length;
var packages   = [];

if (files.length < numWorkers) {
  numWorkers = files.length;
}

if (!process.env.NODE_WORKER_ID) {
  process.env.NODE_WORKER_ID = 'Master';
}

if (cluster.isMaster) {
  // fork workers
  for (var i = 0; i < numWorkers; i++) {
    packages.push([]);
    cluster.fork();
  }

  // distribute the load evenly
  var j = 0;
  while (files.length) {
    packages[j++].push(files.splice(0, 1)[0]);
    j = j % packages.length;
  }

  var result = {};
  function handleMatch(message) {
    var parts   = message.match.split('\t');
    var hood    = parts[1];
    var message = parts[3];

    if (!result[hood]) result[hood] = 0;
    result[hood]++;
  }

  var k = 0;
  cluster.on('online', function(worker) {
    worker.send({ filesToProcess : packages[k++] });
    worker.on('message', handleMatch);
  });

  process.on('exit', function () {
    var finalResult = Object.keys(result)
      .map(function (hood) { return [ hood, result[ hood ] ]; })
      .sort(function (a, b) { return b[ 1 ] - a[ 1 ]; })
      .map(function (pair) { return pair[ 0 ] + "\t" + pair[ 1 ]; })
      .join("\n");
    fs.writeFileSync('./tmp/node_output', finalResult, { encoding : 'utf-8' });
  });

} else {
  process.on('message', function (message) {
    var numFiles = message.filesToProcess.length;
    message.filesToProcess.forEach(function (filePath) {
      var reader = readline.createInterface({
          input    : fs.createReadStream('./tmp/tweets/'+filePath),
          output   : process.stdout,
          terminal : false
      });

      reader.on('line', function(line) {
        if (line.match(/knicks/i)) {
          process.send({ match : line });
        }
      });

      reader.on('close', function () {
        if (--numFiles === 0) process.exit();
      });
    });
  });
}
