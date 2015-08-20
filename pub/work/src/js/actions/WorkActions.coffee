Dispatcher   = require '../dispatcher/Dispatcher.coffee'
Persistence  = require '../persistence/Persistence.coffee'

Persistence.get 'test', console.log.bind(console)
module.exports =
  newItem: (index,list) ->
    item =
      id:window.util.uuid32()
      version:0
      "date-created":Date.now()
      "date-modified":Date.now()
      "due-date":null
      owner:window.urb.ship
      status:'gave'
      tags:[]
      title:''
      description:''
      discussion:[]
    Persistence.put "new":item
    Dispatcher.handleViewAction {type:'newItem', list, index, item}

  swapItems: (to,from,list) ->
    Dispatcher.handleViewAction
      type:'swapItem'
      from:from
      list:list
      to:to

  removeItem: (index,list,id) ->
    Persistence.put old:{id,dif:set:done:Date.now()}
    Dispatcher.handleViewAction
      type:'removeItem'
      index:index
      list:list

  addItem: (index,item,list) ->
    Dispatcher.handleViewAction 
      type:'addItem'
      list:list
      index:index
      item:item
