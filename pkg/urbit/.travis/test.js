'use strict';

var fs      = require('fs')
var runner  = require('urbit-runner')
var Urbit   = runner.Urbit;
var ERROR   = runner.ERROR;
var actions = runner.actions

var args =
  (function() {
    var commit;

    try {
      commit = fs.readFileSync('./pin-arvo-commit.txt', 'utf-8');
    }
    catch (e) {
      return ['-cgSF', 'zod', '-B', 'brass.pill', 'zod'];
    }

    var hash = commit.slice(0, 10);
    var pill = 'https://bootstrap.urbit.org/git-' + hash + '.pill';
    return ['-u', pill, '-cgPSF', 'zod', 'zod'];
  })();

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
//  XX temporary
//  send ctrl-x to select dojo
//
.then(function(){
  return urbit.expect(/talk\[\] /)
  .then(function() {
    return urbit.pty.write("\x18")
  })
  .then(function() { return urbit })
})
.then(actions.safeBoot)
.then(actions.test)
.then(function(){
  return urbit.line("|mass")
  .then(function(){
    return urbit.expectEcho("%ran-mass")
    .then(function(){ return urbit.resetListeners(); })
  })
})
.then(exit)
.catch(function(err){
  // we still exit 0, Arvo errors are not our fault ...
  return urbit.waitSilent()
  .then(function(){
    return urbit.warn("Arvo test aborted:", err);
  })
  .then(exit);
});
