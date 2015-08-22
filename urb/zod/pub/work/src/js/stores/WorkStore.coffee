EventEmitter  = require('events').EventEmitter
assign        = require 'object-assign'
Dispatcher    = require '../dispatcher/Dispatcher.coffee'

_tasks   = 
  "0v0":
    id:"0v0"
    version:0
    sort:0
    date_created:new Date('2015-8-18')
    date_modified:new Date('2015-8-18')
    date_due:new Date('2015-8-18')
    owner:"zod"
    audience:["~doznec/urbit-meta","~doznec/tlon"]
    status:"announced"
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

  "0v1":
    id:"0v1"
    version:0
    sort:1
    date_created:new Date('2015-8-18')
    date_modified:new Date('2015-8-18')
    date_due:null
    owner:"~zod"
    audience:["~doznec/tlon"]
    status:"accepted"
    tags:['home','office']
    title:'eat'
    description:'dont forget about lunch.'
    discussion:[]
    
  "0v2":
    id:"0v2"
    version:0
    sort:2
    date_created:new Date('2015-8-18')
    date_modified:new Date('2015-8-18')
    date_due:null
    owner:"talsur-todres"
    audience:["~doznec/tlon"]
    status:"accepted"
    tags:['home']
    title:'sleep'
    description:'go get some sleep.'
    discussion:[]

_list = ["0v0","0v1","0v2"]
_listening = []
_filters = 
  done:null
  owner:null
  tags:null
  audience:null
  status:null
_sorts =
  sort:0
  title:0
  owner:0
  date_due:0

WorkStore = assign {},EventEmitter.prototype,{
  emitChange: -> @emit 'change'
  addChangeListener: (cb) -> @on 'change', cb
  removeChangeListener: (cb) -> @removeListener "change", cb
  
  getData: ({sort,tasks})->
    sort.map (id,index)=> 
      unless _tasks[id]
        _list.splice index, 0, id
      if tasks[id]  # XX client-side defaults
        _tasks[id] = @itemFromData tasks[id], index
  
  getList: (key) -> 
    list = []
    for id in _list
      task = _tasks[id]
      add = true
      for _k,_v of _filters
        if _v is null then continue
        c = v[_k]
        switch _k
          when 'tags' or 'audience'
            if _.intersection(c,_v).length is 0 then add = false
          when 'done'
            if _v is true and not c then add = false
            if _v is false and c then add = false
          else
            if c isnt _v then add = false
      if add is true
        list.push task
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
  
  canSort: ->
    for k,v of _sorts
      if k is "sort" and v is 1
        return true
      else if v isnt 0
        return false 
    true
    
  itemFromData: (item,index=0)->
    _item = _.extend {sort:index}, item
    _item.date_modified =  new Date item.date_modified
    _item.date_created =   new Date item.date_created
    _item.date_due =       new Date item.date_due if item.date_due?
    _item.done =           new Date item.done if item.done?
    _item.discussion = item.discussion.map ({ship,body,date}) ->
                                            {ship,body,date: new Date date}
    _item

  moveItems: ({list,to,from}) ->
    _tasks[_list[from]].sort = _tasks[_list[to]].sort
    _list = list
  removeItem: ({index}) -> _list.splice index,1

}

WorkStore.setMaxListeners 100

WorkStore.dispatchToken = Dispatcher.register (p) ->
  a = p.action

  if WorkStore[a.type]
    WorkStore[a.type] a
    WorkStore.emitChange()

module.exports = WorkStore
