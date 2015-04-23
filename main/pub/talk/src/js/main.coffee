$(() ->
  StationActions = require './actions/StationActions.coffee'

  rend = React.render
  
  window.chat = {}
  window.chat.MessagePersistence = require './persistence/MessagePersistence.coffee'
  window.chat.StationPersistence = require './persistence/StationPersistence.coffee'

  window.util =
    mainStations: ["court","floor","porch"]
    
    mainStationPath: (user) -> "~#{user}/#{window.util.mainStation(user)}"

    mainStation: (user) ->
      if not user then user = window.urb.user
      switch user.length
        when 3
          return "court"
        when 6
          return "floor"
        when 13
          return "porch"

    clipAudi: (audi) ->
      audi = audi.join " "
      ms = window.util.mainStationPath window.urb.user
      regx = new RegExp "/#{ms}","g"
      audi = audi.replace regx,""
      audi.split " "

    expandAudi: (audi) ->
      audi = audi.join " "
      ms = window.util.mainStationPath window.urb.user
      if audi.indexOf(ms) is -1 
        if audi.length > 0
          audi += " "
        audi += "#{ms}"
      audi.split " "

    create: (name) ->
      window.chat.StationPersistence.createStation name, (err,res) ->
    
    subscribe: (name) ->
      window.chat.StationPersistence.addSource "main",window.urb.ship,["~zod/#{name}"]
    
    uuid32: ->
      str = "0v"
      str += Math.ceil(Math.random()*8)+"."
      for i in [0..5]
        _str = Math.ceil(Math.random()*10000000).toString(32)
        _str = ("00000"+_str).substr(-5,5)
        str += _str+"."
      str.slice(0,-1)

    populate: (station,number) ->
      c = 0
      send = ->
        if c < number
          c++
        else
          console.log 'done'
          return true
        _audi = {}
        _audi[station] = "pending"
        _message =
          serial:window.util.uuid32()
          audience:_audi
          statement:
            speech:
              say:"Message "+c
            time: Date.now()
            now: Date.now()
        window.chat.MessagePersistence.sendMessage _message,send
      send()

    getScroll: ->
      @writingPosition = $('#c').outerHeight(true)+$('#c').offset().top-$(window).height()

    setScroll: ->
      window.util.getScroll()
      $(window).scrollTop($("#c").height())

    checkScroll: ->
      if not window.util.writingPosition
        window.util.getScroll()
      if $(window).scrollTop() < window.util.writingPosition
        $('body').addClass 'scrolling'
      else
        $('body').removeClass 'scrolling'

  # checkScroll = ->
  #   if $(window).scrollTop() > 20
  #     $('#nav').addClass 'scrolling'
  #   else
  #     $('#nav').removeClass 'scrolling'
  # setInterval checkScroll, 500

  so = {}
  so.ls = $(window).scrollTop()
  so.cs = $(window).scrollTop()
  so.w = null
  so.$d = $('#nav > div')
  setSo = -> 
    so.$n = $('#station-container')
    so.w = $(window).width()
    so.h = $(window).height()
    so.dh = $("#c").height()
    so.nh = so.$n.outerHeight(true)
  setSo()
  setInterval setSo,200

  $(window).on 'resize', (e) ->
    if so.w > 1170
      so.$n.removeClass 'm-up m-down m-fixed'

  ldy = 0

  $(window).on 'scroll', (e) -> 
    so.cs = $(window).scrollTop()

    if so.w > 1170
      so.$n.removeClass 'm-up m-down m-fixed'
    if so.w < 1170
      dy = so.ls-so.cs

      if so.cs <= 0
        so.$n.removeClass 'm-up'
        so.$n.addClass 'm-down m-fixed'
        return

      if so.cs+so.h > so.dh then return

      if so.$n.hasClass 'm-fixed' and
      so.w < 1024
        so.$n.css left:-1*$(window).scrollLeft()

      if dy > 0 and ldy > 0
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

      if dy < 0 and ldy < 0
        if not so.$n.hasClass 'm-up'
          so.$n.removeClass 'open'
          so.$n.removeClass('m-down m-fixed').addClass 'm-up'
          so.$n.attr {style:''}
          top = so.cs
          sto = so.$n.offset().top
          if top < 0 then top = 0
          if top > sto and top < sto+so.nh then top = sto
          so.$n.offset top:top

    ldy = dy
    so.ls = so.cs

  $(window).on 'scroll', window.util.checkScroll

  window.chat.StationPersistence.listen()

  StationComponent    = require './components/StationComponent.coffee'
  MessagesComponent   = require './components/MessagesComponent.coffee'
  WritingComponent    = require './components/WritingComponent.coffee'

  $c = $('#c')

  clean = ->
    React.unmountComponentAtNode $('#station-container')[0]
    React.unmountComponentAtNode $('#messages-container')[0]
    React.unmountComponentAtNode $('#writing-container')[0]

  $c.append "<div id='station-container'></div>"
  $c.append "<div id='messages-container'></div>"
  $c.append "<div id='writing-container'></div>"
  $c.append "<div id='scrolling'>BOTTOM</div>"
  rend (React.createElement(StationComponent, {})),$('#station-container')[0]
  rend (React.createElement(MessagesComponent, {})),$('#messages-container')[0]
  rend (React.createElement(WritingComponent, {})),$('#writing-container')[0]
)
