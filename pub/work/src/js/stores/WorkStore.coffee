EventEmitter  = require('events').EventEmitter
assign        = require 'object-assign'
Dispatcher    = require '../dispatcher/Dispatcher.coffee'

_upcoming   = [
    id:"0v0"
    sort:0
    "date-created":new Date('2015-8-18')
    "date-modified":new Date('2015-8-18')
    "date-due":null
    owner:"~talsur-todres"
    audience:["doznec/urbit-meta","doznec/tlon"]
    status:"working"
    tags:['food','office']
    title:'get groceries'
    description:'first go out the door, \n then walk down the block.'
    discussion:[]
  ,
    id:"0v1"
    sort:1
    "date-created":new Date('2015-8-18')
    "date-modified":new Date('2015-8-18')
    "date-due":null
    owner:"~talsur-todres"
    audience:["doznec/tlon"]
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
    audience:["doznec/tlon"]
    status:"working"
    tags:['home']
    title:'sleep'
    description:'go get some sleep.'
    discussion:[]
]
_following  = {}
_incoming   = {}

lists =
  'upcoming':_upcoming
  'following':_following
  'incoming':_incoming

WorkStore = assign {},EventEmitter.prototype,{
  emitChange: -> @emit 'change'
  addChangeListener: (cb) -> @on 'change', cb
  removeChangeListener: (cb) -> @removeListener "change", cb
  
  getList: (key) -> lists[key] 

  newItem: ({index,list,item}) ->
    _item = _.extend {sort:index,audience:[]}, item
    _item["date-created"]=new Date item["date-created"]
    _item["date-modified"]=new Date item["date-modified"]
    _item["date-due"]=item["due-date"]
    lists[list].splice index,0,_item

  swapItem: ({to,from,list}) ->
    list = lists[list]
    list.splice to,0,list.splice(from,1)[0]

  removeItem: ({index,list}) -> 
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
