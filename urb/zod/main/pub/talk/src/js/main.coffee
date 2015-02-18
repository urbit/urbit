$(() ->
    StationActions = require './actions/StationActions.coffee'

    rend = React.render
    
    window.chat = {}
    window.chat.MessagePersistence = require './persistence/MessagePersistence.coffee'
    window.chat.StationPersistence = require './persistence/StationPersistence.coffee'

    window.util =
      mainStation: ->
        switch window.urb.user.length
          when 3
            return "court"
          when 5
            return "floor"
          when 13
            return "porch"

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
        @writingPosition = $('#messaging-container').outerHeight(true)+$('#messaging-container').offset().top-$(window).height()
        #@writingPosition = $('#writing-container').position().top-$(window).height()+$('#writing-container').outerHeight(true)

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
    StationsComponent   = require './components/StationsComponent.coffee'
    MessagesComponent   = require './components/MessagesComponent.coffee'
    WritingComponent    = require './components/WritingComponent.coffee'

    $c = $('#c')

    clean = ->
      React.unmountComponentAtNode $('#stations-container')[0]
      React.unmountComponentAtNode $('#station-parts-container')[0]
      React.unmountComponentAtNode $('#writing-container')[0]
      React.unmountComponentAtNode $('#messages-container')[0]

    routes = 
      '': ->
        clean()
        $c.html "<div id='stations-container'></div>"
        rend (StationsComponent {}, ""),$('#stations-container')[0]
      '/:station': (station) ->
        clean()
        StationActions.switchStation station
        $c.html ""
        $c.append("<div id='messaging-container'></div>")
        $d = $('#messaging-container')
        $d.append("<div id='messages-container'></div>")
        $d.append("<div id='writing-container'></div>")
        $d.append("<div id='station-parts-container'></div>")
        $c.append("<div id='scrolling'>BOTTOM</div>")
        rend (StationComponent {}, ""),$('#station-parts-container')[0]
        rend (MessagesComponent {}, ""),$('#messages-container')[0]
        rend (WritingComponent {}, ""),$('#writing-container')[0]

    router = Router routes
    if not window.location.hash then window.location.hash = "/"
    router.init()
)