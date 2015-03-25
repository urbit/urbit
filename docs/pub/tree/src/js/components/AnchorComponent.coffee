TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

recl = React.createClass
[div,a] = [React.DOM.div,React.DOM.a]

module.exports = recl
  stateFromStore: -> 
    crum:TreeStore.getCrumbs()
    curr:TreeStore.getCurr()
    pare:TreeStore.getPare()
    sibs:TreeStore.getSiblings()
    next:TreeStore.getNext()
    prev:TreeStore.getPrev()
    kids:TreeStore.getKids()
    tree:TreeStore.getTree([])
    cont:TreeStore.getCont()
    url:window.location.pathname

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
    if hist isnt false then history.pushState {}, "", window.tree.basepath href
    TreeActions.setCurr href

  goTo: (path) ->
    @toggleFocus false
    $("html,body").animate {scrollTop:0}
    @setPath path
    if not @checkPath path
      TreeActions.getPath path

  checkURL: ->
    if @state.url isnt window.location.pathname
      @setPath (window.tree.fragpath window.location.pathname),false

  setTitle: ->
    title = $('#cont h1').first().text()
    if title.length is 0
      title = @state.curr.split("/")[@state.curr.split("/").length-1]

    document.title = "#{title} - #{@state.curr}"

  checkUp: ->
    up = @state.curr.split "/"
    up.pop()
    up = up.join "/"
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
      # left
      if e.keyCode is 37
        @goTo @state.prev
      #right 
      if e.keyCode is 39
        @goTo @state.next

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

  _onChangeStore: ->  @setState @stateFromStore()

  render: ->
    parts = []
    if @state.pare
      href = window.tree.basepath @state.pare
      parts.push (div {id:"up"},(a {key:"arow-up",href:href,className:"arow-up"},""))
      if @state.prev or @state.next
        _parts = []
        if @state.prev 
          href = window.tree.basepath window.tree.basepath @state.prev
          _parts.push (a {key:"arow-prev",href:href,className:"arow-prev"},"")
        if @state.next 
          href = window.tree.basepath window.tree.basepath @state.next
          _parts.push (a {key:"arow-next",href:href,className:"arow-next"},"")
        parts.push (div {id:"sides"}, _parts)

    curr = @state.curr

    if _.keys(@state.sibs).length > 0
      p = curr.split "/"
      curr = p.pop()
      up = p.join "/"
      ci=0
      k=0
      sibs = _.map _.keys(@state.sibs).sort(), (i) => 
        c = ""
        if curr is i
          c = "active"
          ci = k
        k++
        href = window.tree.basepath up+"/"+i
        (div {className:c}, (a {key:i+"-a",href:href,onClick:@_click}, i))
      offset = 0
      if ci > 0 then offset = 0
      s = {marginTop:((ci*-24)-offset)+"px"}
      parts.push (div {key:"sibs",id:"sibs",style:s}, sibs)

    obj =
      onMouseOver:@_mouseOver
      onMouseOut:@_mouseOut
      onClick:@_click
      onTouchStart:@_touchStart
      onTouchEnd:@_touchEnd

    if _.keys(window).indexOf("ontouchstart") isnt -1
      delete obj.onMouseOver
      delete obj.onMouseOut

    div obj, parts
