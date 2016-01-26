WorkComponent = require './components/WorkComponent.coffee'
window.util = _.extend window.util || {}, require './util.coffee'

$ ->
  React.render React.createElement(WorkComponent),$('#c')[0]
