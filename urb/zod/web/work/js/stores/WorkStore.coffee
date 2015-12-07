EventEmitter  = require('events').EventEmitter
assign        = require 'object-assign'
Dispatcher    = require '../dispatcher/Dispatcher.coffee'
{uuid32}      = require '../util.coffee'

_tasks   = {}
_list      = []
_listening = []
_updated = Date.now()
_filters = 
  done:null
  creator:null
  doer:null
  audience:null
  tags:null
_sorts =
  sort:0
  title:0
  creator:0
  date_due:0
_ghost = id:uuid32()

WorkStore = assign {},EventEmitter.prototype,{
  emitChange: -> @emit 'change'
  addChangeListener: (cb) -> @on 'change', cb
  removeChangeListener: (cb) -> @removeListener "change", cb
  
  getData: ({sort,tasks})->
    sort.map (id,index)=> 
      unless _tasks[id]
        _list.splice index, 0, id
      if !tasks[id]
        console.log "lost", id
      else if !_tasks[id] or tasks[id].version > _tasks[id].version
        _tasks[id] = @itemFromData tasks[id], index
    _updated = Date.now()

  getUpdated: -> _updated
  
  getFullList: -> _list
  getList: (key) -> 
    list = []
    for id in _list
      task = _tasks[id]
      if !task? or task.archived
        continue
      add = true
      for _k,_v of _filters
        if _v is null then continue
        c = task[_k]
        add = switch _k
          when 'tags', 'audience'
            _.intersection(c,_v).length isnt 0
          when 'creator'
            c is _v.replace(/\~/g, "")
          when 'done'
            !!c is _v
          else c is _v
        break unless add
      if add
        list.push task
    if _.uniq(_.values(_sorts)).length > 1
      for k,v of _sorts
        if v isnt 0
          break
      list = _.sortBy list,k,k
      if v is -1 then list.reverse()
    unless @noNew()
      ghost = $.extend {ghost:true,version:-1}, _ghost
      if _filters.tags     then ghost.tags     = _filters.tags
      if _filters.audience then ghost.audience = _filters.audience
      list.push ghost
    list

  newItem: ({before,after,item}) ->
    if before
      index = _list.indexOf before
      if index is -1 then index = null
    if after
      index = 1 + _list.indexOf after
      if index is 0 then index = null
      
    index ?= _list.length

    if item.id is _ghost.id
      _ghost.id = uuid32()
    unless _tasks[item.id]?
      _list.splice index,0,item.id
    else if _tasks[item.id].version >= 0
      throw new Error "Collision: already have #{item.id}"
    _tasks[item.id] = @itemFromData item, index

  loadFilters: ({filters}) -> 
    console.log 'filters'
    console.log filters
    _filters = filters
  getFilters: -> _filters
  setFilter: ({key,val}) ->
    _filters[key] = val

  loadSorts: ({sorts}) -> 
    console.log 'load sorts'
    console.log sorts
    _sorts = sorts
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
    
  noNew: ->
     (_filters.done is true) or
     _filters.creator? and _filters.owner isnt urb.ship
    
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
  setAudience: ({id,to})-> _tasks[id].audience = to
  archiveItem: ({id})-> _tasks[id].archived = true
  updateItem: ({id,version,key,val})-> 
    _tasks[id].version = version
    _tasks[id].done = val if key is 'done'

}

WorkStore.setMaxListeners 100

WorkStore.dispatchToken = Dispatcher.register (p) ->
  a = p.action

  if WorkStore[a.type]
    WorkStore[a.type] a
    WorkStore.emitChange()

module.exports = WorkStore
