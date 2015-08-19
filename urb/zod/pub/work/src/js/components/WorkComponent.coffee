recl = React.createClass
rece = React.createElement
[div,input,textarea] = [React.DOM.div,React.DOM.input,React.DOM.textarea]

ListComponent = require './ListComponent.coffee'

module.exports = recl
  render: ->
    (div {}, [
      (rece(ListComponent,{list:'upcoming'}))
    ])