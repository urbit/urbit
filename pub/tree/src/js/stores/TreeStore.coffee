EventEmitter = require('events').EventEmitter

MessageDispatcher = require '../dispatcher/Dispatcher.coffee'
clog = console.log

_tree = {}
_cont = {}
_snip = {}; _got_snip = {}
_curr = ""

TreeStore = _.extend EventEmitter.prototype, {
  addChangeListener: (cb) -> @on 'change', cb

  removeChangeListener: (cb) -> @removeListener "change", cb

  emitChange: -> @emit 'change'

  pathToArr: (_path) -> _path.split "/"

  filterQuery: (query)-> @filterWith (@fulfill _curr, query), query
  filterWith: (have,query)->
    return query unless have?
    _query = {}
    for k of query
      _query[k] = query[k] unless have[k]?
    if query.kids? and have.kids?
      if _.isEmpty have.kids
        _query.kids = query.kids
      else
        _query.kids = {}
        for k,kid of have.kids
          _.merge _query.kids, @filterWith kid, query.kids
        if _.isEmpty _query.kids
          delete _query.kids
    _query unless _.isEmpty _query
    
  fulfill: (path,query)->
    data = @fulfillLocal path, query
    if query.body then data.body = _cont[path]
    if query.head then data.head = _snip[path]?.head
    if query.snip then data.snip = _snip[path]?.body
    if query.meta then data.meta = _snip[path]?.meta
    if query.kids
      data.kids = {}
      for k in @getKids path
        data.kids[k] = @fulfill path+"/"+k, query.kids
    data unless _.isEmpty data
      
  fulfillLocal: (path, query)->
    data = {}
    if query.path then data.path = path
    if query.name then data.name = path.split("/").pop()
    if query.sein then data.sein = TreeStore.getPare path
    if query.sibs then data.sibs = TreeStore.getSiblings path
    if query.next then data.next = TreeStore.getNext path
    if query.prev then data.prev = TreeStore.getPrev path
    data
  
  getTree: (_path) ->
    tree = _tree
    for sub in _path
      tree = tree[sub]
      return null unless tree?
    tree

  setCurr: (path) -> _curr = path

  getCurr: -> _curr

  getCont: -> _cont

  mergePathToTree: (path,kids) ->
    tree = _tree
    for sub in @pathToArr path
      tree[sub] = tree[sub] ? {}
      tree = tree[sub]
    for x in kids
      tree[x] = tree[x] ? {}
    tree

  getSnip: -> _snip
  gotSnip: (path)-> !!_got_snip[path]

  loadSnip: (path,kids) ->
    @mergePathToTree path,_.pluck(kids,"name")
    if kids?.length isnt 0
      for v in kids
        _snip[path+"/"+v.name] = 
          head: {gn:'h1',c:v.head}
          body: {gn:'div',c:v.snip}
          meta: v.meta
    else
      _cont[path] =
        gn: 'div'
        c: [ {gn:'h1',  ga:{className:'error'}, c:['Error: Empty path']}
             {gn:'div', c:[
               {gn:'pre',  c:[@getCurr()]}
               {gn:'span', c:['is either empty or does not exist.']}
           ] }]
    _got_snip[path] = true

  loadKids: (path,kids) ->
    @mergePathToTree path,_.pluck(kids,"name")
    for k,v of kids
      _cont[path+"/"+v.name] = v.body

  loadPath: (path,body,kids) ->
    @mergePathToTree path,_.pluck(kids,"name")
    _cont[path] = body

  getKids: (path=_curr)-> _.keys @getTree path.split("/")

  getSiblings: (path=_curr)->
    curr = path.split("/")
    curr.pop()
    if curr.length isnt 0
      @getTree curr
    else
      {}

  getPrev: (path=_curr)-> 
    sibs = _.keys(@getSiblings path).sort()
    if sibs.length < 2
      null
    else
      par = _curr.split "/"
      key = par.pop()
      ind = sibs.indexOf key
      win = if ind-1 >= 0 then sibs[ind-1] else sibs[sibs.length-1]
      par.push win
      par.join "/"

  getNext: (path=_curr)-> 
    sibs = _.keys(@getSiblings path).sort()
    if sibs.length < 2
      null
    else
      par = _curr.split "/"
      key = par.pop()
      ind = sibs.indexOf key
      win = if ind+1 < sibs.length then sibs[ind+1] else sibs[0]
      par.push win
      par.join "/"

  getPare: (path=_curr)-> 
    _path = @pathToArr path
    if _path.length > 1
      _path.pop()
      _path = _path.join "/"
      if _path is "" then _path = "/"
      _path
    else
      null

  getCrumbs: (path=_curr)->
    _path = @pathToArr path
    crum = ""
    crums = []
    for k,v of _path
      crum += "/"+v
      crums.push {name:v,path:crum}
    crums

  getBody: -> if _cont[_curr] then _cont[_curr] else null
}

TreeStore.dispatchToken = MessageDispatcher.register (payload) ->
  action = payload.action

  switch action.type
    when 'path-load'
      TreeStore.loadPath action.path,action.body,action.kids,action.snip
      TreeStore.emitChange()
    when 'snip-load'
      TreeStore.loadSnip action.path,action.kids
      TreeStore.emitChange()
    when 'kids-load'
      TreeStore.loadKids action.path,action.kids
      TreeStore.emitChange()
    when 'set-curr'
      TreeStore.setCurr action.path
      TreeStore.emitChange()

module.exports = TreeStore
