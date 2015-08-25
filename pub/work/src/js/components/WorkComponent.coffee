recl = React.createClass
rece = React.createElement
{div,h1} = React.DOM

ListComponent = require './ListComponent.coffee'

module.exports = recl
  render: ->
    (div {},
      (h1 {className:'leader'}, "Work")
      (rece(ListComponent,{list:'upcoming'}))
    )
