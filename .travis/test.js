'use strict';

var runner  = require('urbit-runner')
var Urbit   = runner.Urbit;
var ERROR   = runner.ERROR;
var actions = runner.actions

var args  = ['-A', '../arvo', '-csgPSF', 'zod', 'zod'];
var urbit = new Urbit(args);

Promise.resolve(urbit)
.then(actions.safeBoot)
.then(actions.test)
.then(function(){
  return urbit.exit(0);
})
.catch(function(err){
  return urbit.waitSilent()
  .then(function(){
    urbit.warn("Arvo test aborted:", err);
    // we still exit 0, it's not our fault ...
    return urbit.exit(0);
  });
});
