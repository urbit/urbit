reactify  = React.createFactory require './Reactify.coffee'
query     = require './Async.coffee'

recl = React.createClass
{div}  = React.DOM

module.exports = query {body:'r',path:'t'}, recl
  displayName: "Body"
  render: -> (div {}, (div {id:'body',key:"body"+@props.path}, 
                       (reactify manx: @props.body)))
