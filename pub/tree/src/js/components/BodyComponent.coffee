clas    = require 'classnames'

query      = require './Async.coffee'
reactify   = require './Reactify.coffee'

recl   = React.createClass
{div,p,img,a}  = React.DOM

Logo = React.createFactory recl 
  render: ->
    {color} = @props
    if color is "white" or color is "black"  # else?
      src = "//storage.googleapis.com/urbit-extra/logo/logo-#{color}-100x100.png"
    (a {href:"http://urbit.org",style:{border:"none"}}, [(img {src,className:"logo"})])
  
Next = React.createFactory query {
    path:'t'
    kids:
      name:'t'
      head:'r'
      meta:'j'
  }, (recl
    displayName: "Links"
    render: ->
      curr = @props.kids[@props.curr]
      if curr?.meta?.next
        keys = window.tree.util.getKeys @props.kids
        if keys.length > 1
          index = keys.indexOf(@props.curr)
          next = index+1
          if next is keys.length then next = 0
          next = keys[next]
          next = @props.kids[next]

          (div {className:"link-next"}, [
            (a {href:"#{@props.path}/#{next.name}"}, "Next: #{next.meta.title}")
          ])
  )

module.exports = query {
  body:'r'
  name:'t'
  path:'t'
  meta:'j'
  sein:'t'
}, recl
  displayName: "Body"
  render: -> 
    className = (@props.meta.layout?.replace /,/g," ") || ""
    body = [reactify @props.body]
    if @props.meta.logo?
      body.unshift (Logo color:@props.meta.logo)
    if @props.meta.next?
      body.push Next {dataPath:@props.sein,curr:@props.name}
    if @props.meta.footer?
      body.push (div {className:"footer"}, [
        (p {}, "This page was served by Urbit.")])
    (div {
        id:'body',
        key:"body"+@props.path
        className
        }, 
      body)
