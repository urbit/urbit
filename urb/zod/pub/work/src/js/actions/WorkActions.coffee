Dispatcher = require '../dispatcher/Dispatcher.coffee'

module.exports =
  newItem: (index,list) ->
    Dispatcher.handleViewAction
      type:'newItem'
      index:index
      list:list

  swapItems: (to,from,list) ->
    Dispatcher.handleViewAction
      type:'swapItem'
      from:from
      list:list
      to:to

  removeItem: (index,list) ->
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