$(() ->
  StationActions = require './actions/StationActions.coffee'

  rend = React.render
  
  window.talk = {}
  window.talk.MessagePersistence = require './persistence/MessagePersistence.coffee'
  window.talk.StationPersistence = require './persistence/StationPersistence.coffee'

  require './util.coffee'
  require './move.coffee'

  # checkScroll = ->
  #   if $(window).scrollTop() > 20
  #     $('#nav').addClass 'scrolling'
  #   else
  #     $('#nav').removeClass 'scrolling'
  # setInterval checkScroll, 500

  window.talk.StationPersistence.listen()

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