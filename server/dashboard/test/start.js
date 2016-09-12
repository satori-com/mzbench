var path = require('path')
var childProcess = require('child_process')
var phantomjs = require('phantomjs')
var binPath = phantomjs.path
 
var childArgs = [
  path.join(__dirname, 'test.js')
]
 
childProcess.execFile(binPath, childArgs, function(err, stdout, stderr) {
  console.log(stdout);
  console.log(stderr);

  if (err) {
    process.exit(err.code);
  }
})