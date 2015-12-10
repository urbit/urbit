clas    = require 'classnames'

load       = require './LoadComponent.coffee'
query      = require './Async.coffee'
reactify   = require './Reactify.coffee'
codemirror = require './CodeMirror.coffee'

TreeActions = require '../actions/TreeActions.coffee'

recl   = React.createClass
rele   = React.createElement
{input,div,pre,p,img,a,button}  = React.DOM

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
    rele codemirror, {value:octs, mode:mite, readOnly:false, autofocus:true}
    
Add = recl
  displayName: "Add"
  getInitialState: -> edit:false
  onClick: -> @setState edit:true
  componentDidUpdate: ->
    if @state.edit
      $(@getDOMNode()).focus()
  render: ->
    unless @state.edit
      button {@onClick}, "Add"
    else
      input {type:"text",onKeyDown:(e)=>
        if 13 is e.keyCode
          {value} = @getDOMNode()
          escp = value.toLowerCase().replace(/[^a-z0-9._~-]+/g, '-')
          newPath = @props.path+"/"+escp
          newSpur = "/"+escp+@props.spur
          history.pushState {}, "", window.tree.basepath newPath + "#edit"
          # TreeActions.setCurr newPath # XX indirect through Anchor poll
          urb.onupdate = -> # disable autoreload
          TreeActions.saveFile newSpur, '# '+value, ->
          TreeActions.loadPath newPath, {
            spur:newSpur
            meta:{}
            mime:{mite:"text/x-markdown",octs:'# '+value}
            body:{gn:"div",ga:{},c:[{gn:"h1",ga:{},c:[value]}]}
          }
          @setState edit:false
      } 

module.exports = query {
  body:'r'
  name:'t'
  path:'t'
  meta:'j'
  sein:'t'
  spur:'t'
}, recl
  displayName: "Body"
  getInitialState: -> edit: document.location.hash is "#edit"
  setEdit: ->
    @hash = document.location.hash
    document.location.hash = "#edit" # XX generic state<->hash binding
    @setState edit:true
    urb.onupdate = -> # disable autoreload
  unsetEdit: ->
    document.location.hash = @hash ? ""
    document.location.reload()  # XX sigh
    # @setState edit:false
  doDelete: ->
    TreeActions.deleteFile @props.spur, => @unsetEdit()
    @setState edit:"gone"
  
  render: -> 
    className = clas (@props.meta.layout?.split ',')
    own = urb.user and urb.user is urb.ship
    extra = (name,props={})=> 
      if @props.meta[name]? then rele extras[name], props
    
    switch @state.edit
      when false
        body = reactify @props.body
        editButton = button {onClick: => @setEdit()}, "Edit"
      when "pending", "gone"
        body = div {}, rele load, {}
        editButton = button {onClick: => @setEdit()}, "Edit"
      when true
        body = rele Edit, {}
        onClick = =>
          txt = $(@getDOMNode()).find('.CodeMirror')[0].CodeMirror.getValue() # XX refs
          TreeActions.saveFile @props.spur, txt, => @unsetEdit()
          @setState edit:"pending"
        editButton = button {onClick}, "Done"

    (div {
        id:'body',
        key:"body"+@props.path
        className
        },
      extra 'spam'
      extra 'logo', color: @props.meta.logo
      if own then editButton
      if own then button {onClick: => @doDelete()}, "Delete"
      body
      extra 'next', {dataPath:@props.sein,curr:@props.name}
      if own then rele Add,{spur:@props.spur, path:@props.path}
      extra 'footer'
    )
