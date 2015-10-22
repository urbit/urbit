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
    displayName: "Links"
    render: -> div {className:'links'}, 
      @props.children, 
      @renderUp(),
      @renderSibs(),
      @renderArrows() 
    renderUp: ->
      if @props.sein 
        div {id:"up",key:"up"}, @renderArrow "up", @props.sein
    renderSibs: ->
      keys = window.tree.util.getKeys @props.kids
      if keys.indexOf(@props.curr) isnt -1
        style = {marginTop: -24 * (keys.indexOf @props.curr) + "px"}
      div {id:"sibs",style}, keys.map (key) =>
        href = window.tree.basepath @props.path+"/"+key
        data = @props.kids[key]
        head = data.meta.title if data.meta
        head ?= @toText data.head
        head ||= key
        className = clas active: key is @props.curr
        (div {className,key}, (a {href,onClick:@props.onClick}, head))
    renderArrow: (name, path) ->
      href = window.tree.basepath path
      (a {href,key:"arow-#{name}",className:"arow-#{name}"},"")
    renderArrows: ->
      keys = window.tree.util.getKeys @props.kids
      if keys.length > 1
        index = keys.indexOf(@props.curr)
        prev = index-1
        next = index+1
        if prev < 0 then prev = keys.length-1
        if next is keys.length then next = 0
        prev = keys[prev]
        next = keys[next]
      if @props.sein
        sein = @props.sein
        if sein is "/" then sein = "" 
        if prev or next then _.filter [
          div {id:"sides",key:"sides"},
            if prev then @renderArrow "prev", "#{sein}/#{prev}"
            if next then @renderArrow "next", "#{sein}/#{next}"
          ]

    toText: (elem)-> reactify.walk elem,
                                 ()->''
                                 (s)->s
                                 ({c})->(c ? []).join ''
  ),  recl
    displayName: "Links_loading"
    render: -> div {className:'links'}, @props.children, @_render()
    _render: -> div {id:"sibs"}, div {className:"active"}, a {}, @props.curr

CLICK = 'a'
module.exports = query {
  sein:'t'
  path:'t'
  name:'t'
  meta:'j'
  },(recl
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

    # $('body').on 'keyup', (e) =>
    #   switch e.keyCode
    #     when 37 then @goTo @props.prev # left
    #     when 39 then @goTo @props.next # right
        
    _this = @
    $('body').on 'click', CLICK, (e) ->
      href = $(@).attr('href')
      id   = $(@).attr('id')
      if href and not /^https?:\/\//i.test(href)
        e.preventDefault()
        e.stopPropagation()
        if href?[0] isnt "/"
          href = (document.location.pathname.replace /[^\/]*\/?$/, '') + href
        _this.goTo window.tree.fragpath href
      if id
        window.location.hash = id

  setTitle: ->
    title = $('#cont h1').first().text() || @props.name
    title = @props.meta.title if @props.meta?.title
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

  reset: ->
    $("html,body").animate {scrollTop:0}
    #  $("#cont").attr 'class',''
    $('#nav').attr 'style',''
    $('#nav').removeClass 'scrolling m-up'
    $('#nav').addClass 'm-down m-fixed'

  goTo: (path) ->
    @toggleFocus false
    @reset()
    @setPath path
  
  checkURL: ->
    if @state.url isnt window.location.pathname
      @reset()
      @setPath (window.tree.fragpath window.location.pathname),false
      @setState url: window.location.pathname
  
  render: ->
    if @props.meta.anchor is 'none' 
      return (div {}, "")

    obj = {@onMouseOver,@onMouseOut,@onClick,@onTouchStart,@onTouchEnd}
    if _.keys(window).indexOf("ontouchstart") isnt -1
      delete obj.onMouseOver
      delete obj.onMouseOut

    div obj, Links {
      @onClick
      curr:@props.name
      dataPath:@props.sein
      sein:@props.sein
    }), div
