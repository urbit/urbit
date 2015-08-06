TreeDispatcher    = require '../dispatcher/Dispatcher.coffee'
TreePersistence   = require '../persistence/TreePersistence.coffee'

module.exports =
  loadPath: (path,body,kids) ->
    TreeDispatcher.handleServerAction
      type:"path-load"
      path:path
      body:body
      kids:kids

  loadKids: (path,kids) ->
    TreeDispatcher.handleServerAction
      type:"kids-load"
      path:path
      kids:kids

  loadSnip: (path,kids) ->
    TreeDispatcher.handleServerAction
      type:"snip-load"
      path:path
      kids:kids

  sendQuery: (path,query) ->
    return unless query?
    if path.slice(-1) is "/" then path = path.slice(0,-1)
   
    TreePersistence.get path,query,(err,res) =>
      switch
        when query.kids?.body then @loadKids path,res.kids
        when query.kids?.head then @loadSnip path,res.kids
        else @loadPath path,res.body,res.kids

  setCurr: (path) ->
    TreeDispatcher.handleViewAction
      type:"set-curr"
      path:path
