'use strict';

var fs      = require('fs')
var runner  = require('urbit-runner')
var Urbit   = runner.Urbit;
var ERROR   = runner.ERROR;
var actions = runner.actions

var args  = ['-B', 'urbit.pill', '-A', '..', '-cSF', 'zod', 'zod'];
var urbit = new Urbit(args);

//  XX upstream the following into runner-js
//
function rePill(urb) {
  return new Promise(function(resolve,reject){
    fs.stat('./built-pill/', function(err, stat) {
      if (err) return resolve()

      fs.readdir('./built-pill/', function(err, files) {
        if (err || (1 !== files.length)) {
          return resolve()
        }

        var name = files[0].replace(/\.pill$/, '')

        urb.note('re-soliding pill')

        return urb.expect(/dojo> /)
        .then(function(){
          return urb.line('|label %home %' + name)
        })
        .then(function(){
          return urb.expect(/dojo> /)
        })
        .then(function(){
          return urb.line('.latest/pill +solid /==/' + name + '/sys')
        })
        .then(function(){
          return urb.expectEcho("%resolid")
        })
        .then(function(){
          return urb.resetListeners();
        })
        .then(function(){
          var write = fs.createWriteStream('./built-pill/' + name + '.pill')
          var read = fs.createReadStream('./zod/.urb/put/latest.pill')

          read.on('error', function(err){
            return reject(err)
          })

          write.on('error', function(err){
            return reject(err)
          })

          write.on('finish', function(){
            return resolve()
          })

          return read.pipe(write)
        })
        .catch(function(err){
          return reject(err)
        });
      })
    })
  })
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
.then(actions.testCores)
.then(actions.testRenderers)
.then(function(){
  return rePill(urbit);
})
.then(function(){
  return urbit.exit(0);
})
.catch(function(err){
  return urbit.waitSilent()
  .then(function(){
    urbit.warn('Test aborted:', err);
    return urbit.exit(1);
  });
});
