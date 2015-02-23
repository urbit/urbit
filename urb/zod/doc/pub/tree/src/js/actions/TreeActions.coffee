TreeDispatcher    = require '../dispatcher/Dispatcher.coffee'
TreePersistence   = require '../persistence/TreePersistence.coffee'

module.exports =
  loadPath: (path,body,kids) ->
    TreeDispatcher.handleServerAction
      type:"path-load"
      path:path
      body:body
      kids:kids

  setLoading: (state) ->
    TreeDispatcher.handleViewAction
      type:"set-load"
      load:state

  loadKids: (path,kids) ->
    TreeDispatcher.handleServerAction
      type:"kids-load"
      path:path
      kids:kids

  loadSnip: (path,snip) ->
    TreeDispatcher.handleServerAction
      type:"snip-load"
      path:path
      snip:snip

  getPath: (path,cb) ->                                          # (path,[query,]cb)
    query = null
    if typeof(cb) is 'string'
      query = arguments[1]
      cb = arguments[2]
    
    if path.slice(-1) is "/" then path = path.slice(0,-1)
    
    TreePersistence.get path,query,(err,res) =>
      switch query
        when "snip"
          @loadSnip path,res.snip
        when "kids"
          @loadKids path,res.kids
        else
          @loadPath path,res.body,res.kids,res.snip
      if cb then cb err,res

  setCurr: (path) ->
    TreeDispatcher.handleViewAction
      type:"set-curr"
      path:path