require './util.coffee'
require './move.coffee'

window.talk =
  Component: require './components/TalkComponent.coffee'
  MessagePersistence: require './persistence/MessagePersistence.coffee'
  StationPersistence: require './persistence/StationPersistence.coffee'
  init: (el) ->
    @StationPersistence.listen()