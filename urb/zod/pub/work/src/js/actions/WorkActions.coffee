Dispatcher   = require '../dispatcher/Dispatcher.coffee'
Persistence  = require '../persistence/Persistence.coffee'
{uuid32}     = require '../util.coffee'

module.exports =
  newItem: ({before,after},_item={}) ->
    item =
      date_created:   Date.now()
      date_modified:  Date.now()
      creator:        window.urb.ship
      version:        -1
      id:             _item.id          ? uuid32()
      date_due:       _item.date_due    ? null
      done:           _item.done        ? null
      doer:           _item.doer        ? null
      tags:           _item.tags        ? []
      title:          _item.title       ? ''
      description:    _item.description ? ''
      discussion:     _item.discussion  ? []
      audience:       _item.audience    ?
        [window.util.talk.mainStationPath window.urb.ship]
    if item.date_due or item.title or item.description
      item.version++
      Persistence.put new:item
    Dispatcher.handleViewAction {type:'newItem', before, after, item}
  
  setItem: ({id,version},key,val) ->
    version += 1
    key = key.split('_').join '-'
    set = "#{key}": val
    Persistence.put old:{id,dif:{set}}
    Dispatcher.handleViewAction {type:'updateItem',id,version,key,val}

  ownItem: ({id,version},act) ->
    version += 1
    Persistence.put old:{id,version,dif:doer:"#{act}":null}

  removeItem: ({id,version}) ->
    if version >= 0
      Persistence.put old:{id,dif:set:audience:[]}
    Dispatcher.handleViewAction {type:'archiveItem',id}
  
  setAudience: ({id},to) ->
    Persistence.put old:{id,dif:set:audience:to}
    Dispatcher.handleViewAction {type:'setAudienece',id,to}

  addComment: ({id,version},val) ->
    version += 1
    Persistence.put old:{id,version,dif:add:comment:val}

  setFilter: (key,val,filters) -> 
    Dispatcher.handleViewAction {type:'setFilter', key,val}
    filters[key] = val
    Persistence.setLocal 'filters',filters

  setSort: (key,val,sorts) -> 
    Dispatcher.handleViewAction {type:'setSort',key,val}
    sorts[key] = val
    Persistence.setLocal 'sorts',sorts

  moveItem: (list,to,from) ->
    Persistence.put {sort:list}
    Dispatcher.handleViewAction {type:'moveItems',list,to,from}

  getLocal: (key)->
    Persistence.getLocal key,(e,r) ->
      new Error(e) if e
      return if r is null
      obj = 
        type:"load#{key[0].toUpperCase()+key.slice(1)}"
      obj[key] = r
      Dispatcher.handleServerAction obj

  listenList: (type)->
    Persistence.subscribe type, (err,d)-> 
      if d?
        {sort,tasks} = d.data
        Dispatcher.handleServerAction {type:"getData",sort,tasks}
