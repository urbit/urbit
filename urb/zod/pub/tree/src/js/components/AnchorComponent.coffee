clas        = require 'classnames'

BodyComponent = React.createFactory require './BodyComponent.coffee'
query       = require './Async.coffee'
reactify    = require './Reactify.coffee'

TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

recl = React.createClass
{div,a} = React.DOM

Links = React.createFactory query {
    path:'t'
    kids:
      name:'t'
      head:'r'
      meta:'j'
  }, (recl
    # {curr:'t',prev:'t,next:'t',onClick:'f'}
    displayName: "Links"
    render: -> div {className:'links'}, @props.children, @_render()
    _render: ->
      sorted = true
      keys = []
      for k,v of @props.kids
        if not v.meta?.sort? then sorted = false
        keys[Number(v.meta?.sort)] = k
      if sorted isnt true
        keys = _(@props.kids).keys().sort()
      style = {marginTop: -24 * (keys.indexOf @props.curr) + "px"}
      div {id:"sibs",style}, keys.map (key) =>
        href = window.tree.basepath @props.path+"/"+key
        data = @props.kids[key]
        head = data.meta.title if data.meta
        head ?= @toText data.head
        head ||= key
        className = clas active: key is @props.curr
        (div {className,key}, (a {href,onClick:@props.onClick}, head))

    toText: (elem)-> reactify.walk elem,
                                 ()->''
                                 (s)->s
                                 ({c})->(c ? []).join ''
  ),  recl
    displayName: "Links_loading"
    render: -> div {className:'links'}, @props.children, @_render()
    _render: -> div {id:"sibs"}, div {className:"active"}, a {}, @props.curr

CLICK = 'a,h1,h2,h3,h4,h5,h6'
module.exports = query {sein:'t',path:'t',name:'t',next:'t',prev:'t'},recl
  displayName: "Anchor"
  getInitialState: -> url: window.location.pathname
  
  onClick: -> @toggleFocus()
  onMouseOver: -> @toggleFocus true
  onMouseOut: -> @toggleFocus false
  onTouchStart: -> @ts = Number Date.now()
  onTouchEnd: -> dt = @ts - Number Date.now()

  toggleFocus: (state) -> $(@getDOMNode()).toggleClass 'focus',state

  componentWillUnmount: -> clearInterval @interval; $('body').off 'click', CLICK
  componentDidUpdate: -> @setTitle()
  componentDidMount: -> 
    @setTitle()
    @interval = setInterval @checkURL,100

    $('body').on 'keyup', (e) =>
      switch e.keyCode
        when 37 then @goTo @props.prev # left
        when 39 then @goTo @props.next # right
        
    _this = @
    $('body').on 'click', CLICK, (e) ->
      href = $(@).attr('href')
      id   = $(@).attr('id')
      if href?[0] is "/"
        e.preventDefault()
        e.stopPropagation()
        _this.goTo window.tree.fragpath href
      else if id
        window.location.hash = id

  setTitle: ->
    title = $('#cont h1').first().text() || @props.name
    document.title = "#{title} - #{@props.path}"

  setPath: (href,hist) ->
    href_parts = href.split("#")
    next = href_parts[0]
    if next.substr(-1) is "/" then next = next.slice(0,-1)
    href_parts[0] = next
    if hist isnt false
      history.pushState {}, "", window.tree.basepath href_parts.join ""
    if next isnt @props.path
      React.unmountComponentAtNode $('#cont')[0]
      TreeActions.setCurr next
      React.render (BodyComponent {}, ""),$('#cont')[0]

  goTo: (path) ->
    @toggleFocus false
    $("html,body").animate {scrollTop:0}
    @setPath path
  
  checkURL: ->
    if @state.url isnt window.location.pathname
      @setPath (window.tree.fragpath window.location.pathname),false
      @setState url: window.location.pathname

  renderArrow: (name, path) ->
    href = window.tree.basepath path
    (a {href,key:"arow-#{name}",className:"arow-#{name}"},"")
  
  render: ->
    obj = {@onMouseOver,@onMouseOut,@onClick,@onTouchStart,@onTouchEnd}
    if _.keys(window).indexOf("ontouchstart") isnt -1
      delete obj.onMouseOver
      delete obj.onMouseOut

    div obj, Links {
      @onClick
      curr:@props.name
      dataPath:@props.sein
    }, if @props.sein then _.filter [
         div {id:"up",key:"up"}, @renderArrow "up", @props.sein
         if @props.prev or @props.next then _.filter [
           div {id:"sides",key:"sides"},
             if @props.prev then @renderArrow "prev", @props.prev
             if @props.next then @renderArrow "next", @props.next
       ] ]
