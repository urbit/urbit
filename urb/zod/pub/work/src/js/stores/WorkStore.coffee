EventEmitter  = require('events').EventEmitter
assign        = require 'object-assign'
Dispatcher    = require '../dispatcher/Dispatcher.coffee'

_list   = [
    id:"0v0"
    sort:0
    "date-created":new Date('2015-8-18')
    "date-modified":new Date('2015-8-18')
    "date-due":new Date('2015-8-18')
    owner:"~zod"
    audience:["~doznec/urbit-meta","~doznec/tlon"]
    status:"working"
    tags:['food','office']
    title:'get groceries'
    description:'first go out the door, \n then walk down the block.'
    discussion:[
      {
        date:new Date('2015-8-18')
        ship:"wictuc-folrex"
        body:"Seems like a great idea."
      }
    ]
  ,
    id:"0v1"
    sort:1
    "date-created":new Date('2015-8-18')
    "date-modified":new Date('2015-8-18')
    "date-due":null
    owner:null
    audience:["~doznec/tlon"]
    status:"working"
    tags:['home','office']
    title:'eat'
    description:'dont forget about lunch.'
    discussion:[]
  ,
    id:"0v2"
    sort:2
    "date-created":new Date('2015-8-18')
    "date-modified":new Date('2015-8-18')
    "date-due":null
    owner:"~talsur-todres"
    audience:["~doznec/tlon"]
    status:"working"
    tags:['home']
    title:'sleep'
    description:'go get some sleep.'
    discussion:[]
]
_listening = []
_filters = 
  owner:null
  tags:null
  audience:null
  status:null
_sorts =
  title:0
  owner:0
  "date-due":0
  sort:0

WorkStore = assign {},EventEmitter.prototype,{
  emitChange: -> @emit 'change'
  addChangeListener: (cb) -> @on 'change', cb
  removeChangeListener: (cb) -> @removeListener "change", cb
  
  getList: (key) -> 
    list = []
    for k,v of _list
      add = true
      for _k,_v of _filters
        if _v is null then continue
        c = v[_k]
        if typeof(c) is 'object'
          if _.intersection(c,_v).length is 0 then add = false
        else
          if c isnt _v then add = false
      if add is true
        list.push v
    if _.uniq(_.values(_sorts)).length > 0
      for k,v of _sorts
        if v isnt 0
          break
      list = _.sortBy list,k,k
      if v is -1 then list.reverse()
    list

  getListening: -> _listening

  getFilters: -> _filters

  setFilter: ({key,val}) -> _filters[key] = val

  getSorts: -> _sorts

  setSort: ({key,val}) -> 
    for k,v of _sorts
      _sorts[k] = 0
    _sorts[key] = val

  newItem: ({index,item}) ->
    _item = _.extend {sort:index}, item
    _item["date-created"]=new Date item["date-created"]
    _item["date-modified"]=new Date item["date-modified"]
    _list.splice index,0,_item

  swapItem: ({to,from}) ->
    _list.splice to,0,_list.splice(from,1)[0]

  removeItem: ({index}) ->  _list.splice index,1

}

WorkStore.setMaxListeners 100

WorkStore.dispatchToken = Dispatcher.register (p) ->
  a = p.action

  if WorkStore[a.type]
    WorkStore[a.type] a
    WorkStore.emitChange()

module.exports = WorkStore
