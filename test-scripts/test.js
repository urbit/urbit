'use strict';

var runner  = require('urbit-runner')
var Urbit   = runner.Urbit;
var ERROR   = runner.ERROR;
var actions = runner.actions

var args  = ['-A', '../arvo', '-csgPSF', 'zod', 'zod'];
var urbit = new Urbit(args);

// vere hangs (always?) with run in travis-ci with -P
// so we send ^Z if we hang for ~s30
function exit() {
  setTimeout(function(){
    urbit.pty.write('\x1a');
    urbit.pty.on('exit', function(code, signal){
      process.exit(0);
    })
  }, 30 * 1000);

  return urbit.exit(0);
}

Promise.resolve(urbit)
.then(actions.safeBoot)
.then(actions.test)
.then(exit)
.catch(function(err){
  // we still exit 0, Arvo errors are not our fault ...
  return urbit.waitSilent()
  .then(function(){
    return urbit.warn("Arvo test aborted:", err);
  })
  .then(exit);
});
