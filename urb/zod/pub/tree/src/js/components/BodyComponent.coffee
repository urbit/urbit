clas    = require 'classnames'

query      = require './Async.coffee'
reactify   = require './Reactify.coffee'

recl   = React.createClass
{div,p,img,a}  = React.DOM

extras = 
  footer: recl
    displayName: "Footer"
    render: ->
      (div {className:"footer"}, (p {}, "This page was served by Urbit."))
  logo: recl 
    displayName:"Logo"
    render: ->
      {color} = @props
      if color is "white" or color is "black"  # else?
        src = "//storage.googleapis.com/urbit-extra/logo/logo-#{color}-100x100.png"
      (a {href:"http://urbit.org",style:{border:"none"}}, 
       (img {src,className:"logo"})
      )

  spam: recl
    displayName:"Spam"
    render: ->
      if document.location.hostname isnt 'urbit.org'
        return (div {})
      (div {className:'spam'},
        (a {href:"http://urbit.org#sign-up"}, "Sign up")
        " for an Urbit invite."
      )

  next: query {
    path:'t'
    kids:
      name:'t'
      head:'r'
      meta:'j'
  }, recl
    displayName: "Next"
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

          if next
            return (div {className:"link-next"}, [
              (a {href:"#{@props.path}/#{next.name}"}, "Next: #{next.meta.title}")
            ])
      return (div {},"")

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
    body = [reactify @props.body, "body"]
    
    extra = (name,pos,props={})=> 
      props.key = name
      if @props.meta[name]?
        body[pos] React.createElement extras[name], props
    extra 'spam', 'unshift'
    extra 'logo', 'unshift', color: @props.meta.logo
    extra 'next', 'push', {dataPath:@props.sein,curr:@props.name}
    extra 'footer', 'push'
    
    (div {
        id:'body',
        key:"body"+@props.path
        className
        }, 
      body)
