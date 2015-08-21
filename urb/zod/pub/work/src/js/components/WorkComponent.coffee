recl = React.createClass
rece = React.createElement
[div,h1] = [React.DOM.div,React.DOM.h1]

ListComponent = require './ListComponent.coffee'

module.exports = recl
  render: ->
    (div {}, [
      (h1 {className:'leader'}, "Work")
      (rece(ListComponent,{list:'upcoming'}))
    ])