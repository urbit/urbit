EventEmitter  = require('events').EventEmitter
assign        = require 'object-assign'
Dispatcher    = require '../dispatcher/Dispatcher.coffee'

_tasks   = {}
_list      = []
_listening = []
_updated = Date.now()
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
    _updated = Date.now()

  getUpdated: -> _updated
  
  getList: (key) -> 
    list = []
    for id in _list
      task = _tasks[id]
      if task.archived
        continue
      add = true
      for _k,_v of _filters
        if _v is null then continue
        c = task[_k]
        add = switch _k
          when 'tags', 'audience'
            _.intersection(c,_v).length isnt 0
          when 'owner'
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
    list

  newItem: ({index,item}) ->
    _list.splice index,0,item.id
    _tasks[item.id] = item

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
  setAudience: ({id,to})-> _tasks[id].audience = to
  archiveItem: ({id})-> _tasks[id].archived = true

}

WorkStore.setMaxListeners 100

WorkStore.dispatchToken = Dispatcher.register (p) ->
  a = p.action

  if WorkStore[a.type]
    WorkStore[a.type] a
    WorkStore.emitChange()

module.exports = WorkStore
