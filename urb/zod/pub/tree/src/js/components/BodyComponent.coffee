query      = require './Async.coffee'
reactify   = require './Reactify.coffee'

recl   = React.createClass
{div}  = React.DOM

module.exports = query {body:'r',path:'t',meta:'j'}, recl
  displayName: "Body"
  render: -> 
    $("#cont").attr 'class',''
    if @props.meta.layout
      $("#cont").attr 'class',@props.meta.layout.replace /,/g," "
    (div {}, 
      (div {
          id:'body',
          key:"body"+@props.path}, 
        reactify @props.body)
    )
