rend = React.render

$ ->
  $body = $('body')

  React.initializeTouchEvents(true)

  head = React.createFactory require './components/AnchorComponent.coffee'
  body = React.createFactory require './components/BodyComponent.coffee'
  window.tree.components = require './components/Components.coffee' # sigh
  
  window.tree._basepath = window.urb.util.basepath("/")
  window.tree._basepath +=
    (window.location.pathname.replace window.tree._basepath, "").split("/")[0]
  window.tree.basepath = (path) -> 
    prefix = window.tree._basepath
    if prefix is "/" then prefix = ""
    if path[0] isnt "/" then path = "/"+path
    _path = prefix + path
    if _path.slice(-1) is "/" then _path = _path.slice(0,-1)
    _path
  window.tree.fragpath = (path) ->
    path.replace(/\/$/,'')
        .replace(window.tree._basepath,"")

  TreeActions       = require './actions/TreeActions.coffee'
  TreePersistence   = require './persistence/TreePersistence.coffee'

  frag = window.tree.fragpath window.location.pathname.replace /\.[^\/]*$/, ''

  TreeActions.setCurr frag
  TreeActions.loadPath frag,window.tree.data

  rend (head {}, ""),$('#nav')[0]
  rend (body {}, ""),$('#cont')[0]

  window.tree.util = 
    getKeys: (kids) ->
      sorted = true
      keys = []
      for k,v of kids
        continue if v.meta?.hide
        if not v.meta?.sort? then sorted = false
        keys[Number(v.meta?.sort)] = k
      if sorted isnt true
        keys = _.keys(kids).sort()
      else
        keys = _.values keys

  checkScroll = ->
    if $(window).scrollTop() > 20
      $('#nav').addClass 'scrolling'
    else
      $('#nav').removeClass 'scrolling'
  setInterval checkScroll, 500

  po = {}
  po.cm = null
  po.lm = null
  po.cs = $(window).scrollTop()
  po.ls = $(window).scrollTop()
  $(document).mousemove (e) -> po.cm = {x:e.pageX, y:e.pageY}
  checkMove = ->
    if po.lm isnt null and po.cm isnt null
      po.cs = $(window).scrollTop()

      db = $(window).height()-(po.cs+window.innerHeight)

      ds = Math.abs po.cs-po.ls
      dx = Math.abs po.cm.x-po.lm.x
      dy = Math.abs po.cm.y-po.lm.y
      
      $('#nav').toggleClass 'moving',(dx > 20 or dy > 20 or db < 180)
    po.lm = po.cm
    po.ls = po.cs
  setInterval checkMove,200

  so = {}
  so.ls = $(window).scrollTop()
  so.cs = $(window).scrollTop()
  so.w = null
  so.$n = $('#nav')
  so.$d = $('#nav > div')
  so.nh = $('#nav').outerHeight(true)
  setSo = ->
    so.w = $(window).width()
    so.$n = $('#nav')
  setInterval setSo,200

  $(window).on 'resize', (e) ->
    if so.w > 1170
      so.$n.removeClass 'm-up m-down m-fixed'

  $(window).on 'scroll', (e) -> 
    so.cs = $(window).scrollTop()

    if so.w > 1170
      so.$n.removeClass 'm-up m-down m-fixed'
    if so.w < 1170
      dy = so.ls-so.cs

      so.$d.removeClass 'focus'

      if so.cs <= 0
        so.$n.removeClass 'm-up'
        so.$n.addClass 'm-down m-fixed'
        return

      if so.$n.hasClass 'm-fixed' and
      so.w < 1024
        so.$n.css left:-1*$(window).scrollLeft()

      if dy > 0
        if not so.$n.hasClass 'm-down'
          so.$n.removeClass('m-up').addClass 'm-down'
          top = so.cs-so.nh
          if top < 0 then top = 0
          so.$n.offset top:top
        if so.$n.hasClass('m-down') and 
        not so.$n.hasClass('m-fixed') and 
        so.$n.offset().top >= so.cs
          so.$n.addClass 'm-fixed'
          so.$n.attr {style:''}

      if dy < 0
        if not so.$n.hasClass 'm-up'
          so.$n.removeClass('m-down m-fixed').addClass 'm-up'
          so.$n.attr {style:''}
          top = so.cs
          sto = so.$n.offset().top
          if top < 0 then top = 0
          if top > sto and top < sto+so.nh then top = sto
          so.$n.offset top:top

    so.ls = so.cs
