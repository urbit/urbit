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
        for v in window.util.mainStations
          regx = new RegExp "/#{v}","g"
          audi = audi.replace regx,""
        audi.split " "

      expandAudi: (audi) ->
        for k,v of audi
          if v.indexOf("/") is -1
            audi[k] = "#{v}/#{window.util.mainStation(v.slice(1))}"
        audi

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
        $(window).scrollTop(window.util.writingPosition)

      checkScroll: ->
        if not window.util.writingPosition
          window.util.getScroll()
        if $(window).scrollTop() < window.util.writingPosition
          $('body').addClass 'scrolling'
        else
          $('body').removeClass 'scrolling'

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
    rend (StationComponent {}, ""),$('#station-container')[0]
    rend (MessagesComponent {}, ""),$('#messages-container')[0]
    rend (WritingComponent {}, ""),$('#writing-container')[0]
)