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

  getPath: (path,endpoint="") ->
    
    if path.slice(-1) is "/" then path = path.slice(0,-1)
    
    query={                           # XX unify
      "":   "body.r__kids_name.t"
      kids: "kids_name.t_body.r"
      snip: "kids_name.t_snip.r_head.r_meta.j"
    }[endpoint]

    TreePersistence.get path,query,(err,res) =>
      switch endpoint
        when "snip" then @loadSnip path,res.kids
        when "kids" then @loadKids path,res.kids
        else @loadPath path,res.body,res.kids

  setCurr: (path) ->
    TreeDispatcher.handleViewAction
      type:"set-curr"
      path:path
