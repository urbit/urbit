EventEmitter  = require('events').EventEmitter
assign        = require 'object-assign'
Dispatcher    = require '../dispatcher/Dispatcher.coffee'

_list   = [
    id:0
    sort:0
    "date-created":new Date('2015-8-18')
    "date-modifed":new Date('2015-8-18')
    "date-due":null
    owner:"~talsur-todres"
    audience:["doznec/urbit-meta","doznec/tlon"]
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
    id:1
    sort:1
    "date-created":new Date('2015-8-18')
    "date-modifed":new Date('2015-8-18')
    "date-due":null
    owner:"~talsur-todres"
    audience:["doznec/tlon"]
    status:"working"
    tags:['home','office']
    title:'eat'
    description:'dont forget about lunch.'
    discussion:[]
  ,
    id:2
    sort:2
    "date-created":new Date('2015-8-18')
    "date-modifed":new Date('2015-8-18')
    "date-due":null
    owner:"~talsur-todres"
    audience:["doznec/tlon"]
    status:"working"
    tags:['home']
    title:'sleep'
    description:'go get some sleep.'
    discussion:[]
]
_listening = []
_filters = 
  owned:null
  tag:null
  channel:null
  status:null
_sorts =
  name:null
  owner:null
  date:null
  priority:null

WorkStore = assign {},EventEmitter.prototype,{
  emitChange: -> @emit 'change'
  addChangeListener: (cb) -> @on 'change', cb
  removeChangeListener: (cb) -> @removeListener "change", cb
  
  getList: (key) -> 
    list = []
    for k,v of _list
      add = true
      if _filters.owned isnt null
        if v.owner isnt _filters.owned
          add = false
      if add is true
        list.push v
    list

  getListening: -> _listening

  getFilters: -> _filters

  setFilter: ({key,val}) -> _filters[key] = val

  getSorts: -> _sorts

  setSort: ({key,val}) -> _sorts[key] = val

  newItem: ({index}) ->
    item =
      id:index
      sort:index
      "date-created":new Date()
      "date-modifed":new Date()
      "date-due":null
      owner:"~talsur-todres"
      status:null
      tags:[]
      audience:[]
      title:''
      description:''
      discussion:[]
    _list.splice index,0,item

  swapItem: ({to,from}) ->
    _list.splice to,0,_list.splice(from,1)[0]

  removeItem: ({index}) -> 
    list = lists[list]
    list.splice index,1

}

WorkStore.setMaxListeners 100

WorkStore.dispatchToken = Dispatcher.register (p) ->
  a = p.action

  if WorkStore[a.type]
    WorkStore[a.type] a
    WorkStore.emitChange()

module.exports = WorkStore
