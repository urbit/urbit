BodyComponent = require './BodyComponent.coffee'

TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

recl = React.createClass
[div,a] = [React.DOM.div,React.DOM.a]

module.exports = recl
  displayName: "Anchor"
  stateFromStore: ->
    { 
      path:TreeStore.getCurr()
      pare:TreeStore.getPare()
      sibs:TreeStore.getSiblings()
      snip:TreeStore.getSnip()
      next:TreeStore.getNext()
      prev:TreeStore.getPrev()
      kids:TreeStore.getKids()
      tree:TreeStore.getTree([])
      cont:TreeStore.getCont()
      url:window.location.pathname
    }

  checkPath: (path) -> @state.cont[path]?

  toggleFocus: (state) ->
    $(@getDOMNode()).toggleClass 'focus',state

  _click: -> @toggleFocus()

  _mouseOver: -> @toggleFocus true

  _mouseOut: -> @toggleFocus false

  _touchStart: ->
    @ts = Number Date.now()

  _touchEnd: ->
    dt = @ts - Number Date.now()

  setPath: (href,hist) ->
    href_parts = href.split("#")
    next = href_parts[0]
    if next.substr(-1) is "/" then next = next.slice(0,-1)
    href_parts[0] = next
    if hist isnt false then history.pushState {}, "", window.tree.basepath href_parts.join ""
    rend = false
    if next isnt @state.path
      React.unmountComponentAtNode $('#cont')[0]
      rend = true
    TreeActions.setCurr next
    if rend is true
      React.render (BodyComponent {}, ""),$('#cont')[0]

  goTo: (path) ->
    @toggleFocus false
    $("html,body").animate {scrollTop:0}
    frag = path.split("#")[0]
    @setPath path
    if not @checkPath frag
      TreeActions.getPath frag

  checkURL: ->
    if @state.url isnt window.location.pathname
      @setPath (window.tree.fragpath window.location.pathname),false

  setTitle: ->
    title = $('#cont h1').first().text()
    if title.length is 0
      path = @state.path.split("/")
      title = path[path.length-1]

    document.title = "#{title} - #{@state.path}"

  checkUp: ->
    up = @state.pare ? "/"
    if up.slice(-1) is "/" then up = up.slice 0,-1

    if not @state.cont[up]
      TreeActions.getPath up
        
  componentDidUpdate: -> 
    @setTitle()
    @checkUp()

  componentDidMount: -> 
    TreeStore.addChangeListener @_onChangeStore

    @setTitle()
    @checkUp()

    @interval = setInterval @checkURL,100

    $('body').on 'keyup', (e) =>
      switch e.keyCode
        when 37 then @goTo @state.prev # left
        when 39 then @goTo @state.next # right

    $('body').on 'click', 'a', (e) =>
      href = $(e.target).closest('a').attr('href')
      if href[0] is "/"
        e.preventDefault()
        e.stopPropagation()
        @goTo window.tree.fragpath href

  componentWillUnmount: -> 
    clearInterval @interval
    $('body').off 'click', 'a'

  getInitialState: -> @stateFromStore()

  _onChangeStore: -> @setState @stateFromStore()

  renderArrow: (name, path) ->
    href = window.tree.basepath path
    (a {href,key:"arow-#{name}",className:"arow-#{name}"},"")
  
  renderParts: -> [
    if @state.pare then _.filter [
      div {id:"up",key:"up"}, @renderArrow "up", @state.pare
      if @state.prev or @state.next
        div {id:"sides",key:"sides"}, _.filter [
          if @state.prev then @renderArrow "prev", @state.prev
          if @state.next then @renderArrow "next", @state.next
    ] ]
    if _.keys(@state.sibs).length > 0
      [up..., curr] = @state.path.split "/"
      up = up.join "/"
      ci=0
      k=0
      _sibs = _(@state.sibs).keys().sort().map (i) => 
        if curr is i
          className = "active"
          ci = k
        className ?= ""
        k++
        path = up+"/"+i
        href = window.tree.basepath path
        head = @state.snip[path]?.head ? div {}, i
        head = $(React.renderToStaticMarkup head).text()
        (div {className,key:i}, (a {href,onClick:@_click}, head))
      style = {marginTop:"#{-24*ci}px"}
      div {key:"sibs",id:"sibs",style}, _sibs
    ]
  
  render: ->
    obj =
      onMouseOver:@_mouseOver
      onMouseOut:@_mouseOut
      onClick:@_click
      onTouchStart:@_touchStart
      onTouchEnd:@_touchEnd

    if _.keys(window).indexOf("ontouchstart") isnt -1
      delete obj.onMouseOver
      delete obj.onMouseOut

    div obj, _.filter @renderParts()
