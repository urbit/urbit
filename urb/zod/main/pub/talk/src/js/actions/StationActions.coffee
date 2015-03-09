StationDispatcher = require '../dispatcher/Dispatcher.coffee'

module.exports =
  loadConfig: (station,config) ->
    StationDispatcher.handleServerAction
      type: "config-load"
      station:station
      config:config

  switchStation: (station) ->
    StationDispatcher.handleViewAction
      type:"station-switch"
      station:station

  setAudience: (audience) ->
    StationDispatcher.handleViewAction
      type:"station-set-audience"
      audience:audience

  setValidAudience: (valid) ->
    StationDispatcher.handleViewAction
      type:"station-set-valid-audience"
      valid:valid

  toggleAudience: (station) ->
    StationDispatcher.handleViewAction
      type:"station-audience-toggle"
      station:station    

  removeStation: (station) ->
    window.chat.StationPersistence.removeStation station

  setSources: (station,sources) ->
    window.chat.StationPersistence.setSources station,window.urb.ship,sources

  createStation: (name) ->
    window.chat.StationPersistence.createStation name

  listenStation: (station) ->
    window.chat.StationPersistence.listenStation station

  listeningStation: (station) ->
    StationDispatcher.handleViewAction
      type:"station-listen"
      station:station

  setTyping: (station,state) ->
    StationDispatcher.handleViewAction
      type:"typing-set"
      station:station
      state:state

  ping: (_ping) ->
    window.chat.StationPersistence.ping _ping

  loadStations: (stations) ->
    StationDispatcher.handleServerAction
      type:"stations-load"
      stations:stations

  loadMembers: (members) ->
    StationDispatcher.handleServerAction
      type:"members-load"
      members:members

  createStation: (station) ->
    StationDispatcher.handleViewAction
      type: "station-create"
      station: station
    window.chat.StationPersistence.createStation station