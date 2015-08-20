Dispatcher = require '../dispatcher/Dispatcher.coffee'

module.exports =
  setFilter: (key,val) ->
    Dispatcher.handleViewAction
      type:'setFilter'
      key:key
      val:val

  setSort: (key,val) ->
    Dispatcher.handleViewAction
      type:'setSort'
      key:key
      val:val

  newItem: (index) ->
    Dispatcher.handleViewAction
      type:'newItem'
      index:index

  swapItems: (to,from) ->
    Dispatcher.handleViewAction
      type:'swapItem'
      from:from
      to:to

  removeItem: (index) ->
    Dispatcher.handleViewAction
      type:'removeItem'
      index:index

  addItem: (index,item) ->
    Dispatcher.handleViewAction
      type:'addItem'
      index:index
      item:item