TreeDispatcher    = require '../dispatcher/Dispatcher.coffee'
TreePersistence   = require '../persistence/TreePersistence.coffee'

module.exports =
  loadPath: (path,data) ->
    TreeDispatcher.handleServerAction {path,data,type:"path-load"}

  sendQuery: (path,query) ->
    return unless query?
    if path.slice(-1) is "/" then path = path.slice(0,-1)
    TreePersistence.get path,query,(err,res) => 
      if err? then throw err
      @loadPath path,res

  setCurr: (path) ->
    TreeDispatcher.handleViewAction
      type:"set-curr"
      path:path

  saveFile: (spur, mime, cb) ->
    TreePersistence.put spur, mime, (err,res)->
      if err? then throw err
      if cb? then cb res
      
  deleteFile: (spur, cb) ->
    TreePersistence.del spur, (err,res)->
      if err? then throw err
      if cb? then cb res
