query      = require './Async.coffee'
reactify   = require './Reactify.coffee'

recl   = React.createClass
{div,p,img}  = React.DOM

module.exports = query {body:'r',path:'t',meta:'j'}, recl
  displayName: "Body"
  render: -> 
    $("#cont").attr 'class',''
    if @props.meta.layout
      $("#cont").attr 'class',@props.meta.layout.replace /,/g," "
    body = [reactify @props.body]
    if @props.meta.logo?
      body.unshift((img {className:"logo #{@props.meta.logo}"}, ""))
    if @props.meta.footer?
      body.push((div {className:"footer"}, [
        (p {}, "This page was served by Urbit.")])
      )
    (div {
        id:'body',
        key:"body"+@props.path}, 
      body)
