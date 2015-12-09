clas    = require 'classnames'

query      = require './Async.coffee'
reactify   = require './Reactify.coffee'
codemirror = require './CodeMirror.coffee'

recl   = React.createClass
rele   = React.createElement
{div,pre,p,img,a,button}  = React.DOM

extras =
  spam: recl
    displayName:"Spam"
    render: ->
      if document.location.hostname isnt 'urbit.org'
        return (div {})
      (div {className:'spam'},
        (a {href:"http://urbit.org#sign-up"}, "Sign up")
        " for our newsletter."
      )

  logo: recl 
    displayName:"Logo"
    render: ->
      {color} = @props
      if color is "white" or color is "black"  # else?
        src = "//storage.googleapis.com/urbit-extra/logo/logo-#{color}-100x100.png"
      (a {href:"http://urbit.org",style:{border:"none"}}, 
       (img {src,className:"logo"})
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

  footer: recl
    displayName: "Footer"
    render: ->
      (div {className:"footer"}, (p {}, "This page was served by Urbit."))

Edit = query {mime:'m'}, recl
  displayName: "Edit"
  render: ->
    {mite,octs} = @props.mime
    codemirror {value:octs, readOnly:false, mode:mite}

module.exports = query {
  body:'r'
  name:'t'
  path:'t'
  meta:'j'
  sein:'t'
}, recl
  displayName: "Body"
  getInitialState: -> edit:false
  render: -> 
    className = clas (@props.meta.layout?.split ',')
    own = urb.ship is urb.user
    extra = (name,props={})=> 
      if @props.meta[name]? then rele extras[name], props
    
    body =
      if @state.edit
        rele Edit, {onFinish:=> @setState edit:false}
      else reactify @props.body

    (div {
        id:'body',
        key:"body"+@props.path
        className
        },
      extra 'spam'
      extra 'logo', color: @props.meta.logo
      if own 
        button {onClick: => @setState edit:true}, "Edit"
      body
      extra 'next', {dataPath:@props.sein,curr:@props.name}
      if own then button {}, "Add"
      extra 'footer'
    )
