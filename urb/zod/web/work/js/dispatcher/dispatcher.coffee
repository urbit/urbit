Dispatcher = require('flux').Dispatcher

module.exports = _.merge new Dispatcher(), {
  handleServerAction: (action) ->
    @dispatch
      source: 'server'
      action: action

  handleViewAction: (action) ->
    @dispatch
      source: 'view'
      action: action
}