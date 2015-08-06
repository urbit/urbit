reactify    = (manx)-> React.createElement window.tree.reactify, {manx}
query       = require './Async.coffee'

recl = React.createClass
{div,a,ul,li,hr} = React.DOM

module.exports = query {kids: body:'r'}, recl
  displayName: "Kids"
  render: -> div {className:"kids"},
    for v in _.keys(@props.kids).sort()
      [(div {key:v}, reactify @props.kids[v].body), (hr {},"")]
