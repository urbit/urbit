query      = require './Async.coffee'
reactify   = require './Reactify.coffee'

recl   = React.createClass
{div}  = React.DOM

module.exports = query {body:'r',path:'t'}, recl
  displayName: "Body"
  render: -> (div {}, (div {id:'body',key:"body"+@props.path}, reactify @props.body))
