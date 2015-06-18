EventEmitter = require('events').EventEmitter

MessageDispatcher = require '../dispatcher/Dispatcher.coffee'

_tree = {}
_cont = {}
_snip = {}
_load = false
_curr = ""

TreeStore = _.extend EventEmitter.prototype, {
  addChangeListener: (cb) -> @on 'change', cb

  removeChangeListener: (cb) -> @removeListener "change", cb

  emitChange: -> @emit 'change'

  pathToArr: (_path) -> _path.split "/"

  pathToObj:(_path,_obj,kids) ->
    __path = @pathToArr _path
    for i in [0..__path.length-1]
      _obj = _obj[__path[i]] = {}
    if kids?.length > 0
      for i in [0..kids.length-1]
        _obj[kids[i]] = {}

  getTree: (_path) ->
    tree = _tree
    if _path.length > 0
      for i in [0.._path.length-1]
        if tree[_path[i]]
          tree = tree[_path[i]]
        else
          return null
    tree

  setCurr: (path) -> _curr = path

  getCurr: -> _curr

  getCont: -> _cont

  setLoad: (load) -> _load = load

  getLoad: -> _load

  mergePathToTree: (path,kids) ->
    _obj = {}
    @pathToObj path,_obj,kids
    _.merge _tree,_obj

  getSnip: -> _snip

  loadSnip: (path,snip) ->
    @mergePathToTree path,_.pluck(snip,"name")
    if snip?.length isnt 0
      for k,v of snip
        _snip[path+"/"+v.name] = 
          head: window.tree.reactify v.body.head
          body: window.tree.reactify v.body.body
    else
      _cont[path] = window.tree.reactify "React.createElement ('div', {}, [
                                           React.createElement('h1', {className:'error'}, 'Error: Empty path'),
                                           React.createElement('div', {}, [
                                            React.createElement('pre', {}, '#{@getCurr()}'),
                                            React.createElement('span', {}, 'is either empty or does not exist.')
                                           ]) ])"

  loadKids: (path,kids) ->
    @mergePathToTree path,_.pluck(kids,"name")
    for k,v of kids
      _cont[path+"/"+v.name] = window.tree.reactify v.body

  loadPath: (path,body,kids) ->
    @mergePathToTree path,kids
    _cont[path] = window.tree.reactify body

  getKids: -> _.keys @getTree _curr.split("/")

  getSiblings: ->
    curr = _curr.split("/")
    curr.pop()
    if curr.length isnt 0
      @getTree curr
    else
      {}

  getPrev: -> 
    sibs = _.keys(@getSiblings()).sort()
    if sibs.length < 2
      null
    else
      par = _curr.split "/"
      key = par.pop()
      ind = sibs.indexOf key
      win = if ind-1 >= 0 then sibs[ind-1] else sibs[sibs.length-1]
      par.push win
      par.join "/"

  getNext: -> 
    sibs = _.keys(@getSiblings()).sort()
    if sibs.length < 2
      null
    else
      par = _curr.split "/"
      key = par.pop()
      ind = sibs.indexOf key
      win = if ind+1 < sibs.length then sibs[ind+1] else sibs[0]
      par.push win
      par.join "/"

  getPare: -> 
    _path = @pathToArr _curr
    if _path.length > 1
      _path.pop()
      _path = _path.join "/"
      if _path is "" then _path = "/"
      _path
    else
      null

  getCrumbs: ->
    _path = @pathToArr _curr
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
      TreeStore.loadSnip action.path,action.snip
      TreeStore.emitChange()
    when 'kids-load'
      TreeStore.loadKids action.path,action.kids
      TreeStore.emitChange()
    when 'set-curr'
      TreeStore.setCurr action.path
      TreeStore.emitChange()
    when 'set-load'
      TreeStore.setLoad action.load
      TreeStore.emitChange()

module.exports = TreeStore
