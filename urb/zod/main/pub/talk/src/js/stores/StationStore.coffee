EventEmitter = require('events').EventEmitter

StationDispatcher = require '../dispatcher/Dispatcher.coffee'

_audience = []
_members = {}
_stations = []
_listening = []
_station = null
_config = {}
_typing = {}

StationStore = _.merge new EventEmitter,{
  removeChangeListener: (cb) -> @removeListener "change", cb

  emitChange: -> @emit 'change'

  addChangeListener: (cb) -> @on 'change', cb

  getAudience: -> _audience

  setAudience: (audience) -> _audience = audience

  toggleAudience: (station) ->
    if _audience.indexOf(station) isnt -1
      _audience.splice _audience.indexOf(station), 1
    else
      _audience.push station

  loadConfig: (station,config) -> _config[station] = config

  getConfigs: -> _config

  getConfig: (station) -> _config[station]

  getMember: (ship) -> {ship:ship}

  changeMember: (dir,name,ship) ->
    if dir is "out"
      _members = _.filter _members, (_member) ->
        return (_member.ship isnt ship)
    if dir is "in"
      _members.push {name:name, ship:ship}

  loadMembers: (station,members) -> _members[station] = members

  getMembers: -> _members

  getListening: -> _listening

  setListening: (station) -> 
    if _listening.indexOf(station) isnt -1
      console.log 'already listening on that station (somehow).'
    else
      _listening.push station

  createStation: (station) ->
    _stations.push(station) if _stations.indexOf(station) is -1

  loadStations: (stations) -> _stations = stations 

  getStations: -> _stations

  setStation: (station) -> _station = station

  unsetStation: (station) -> 
    _station = null if _station is station 

  getStation: -> _station

  joinStation: (station) ->
    if _config.court?.sources.indexOf(station) is -1
      _config.court.sources.push station

  getTyping: () -> _typing

  setTyping: (station,state) -> 
    for k,v of _typing
      _typing[k] = (k is station)
    _typing[station] = state
}

StationStore.setMaxListeners 100

StationStore.dispatchToken = StationDispatcher.register (payload) ->
  action = payload.action

  switch action.type
    when 'station-audience-toggle'
      StationStore.toggleAudience action.station
      StationStore.emitChange()
      break
    when 'station-set-audience'
      StationStore.setAudience action.audience
      StationStore.emitChange()
      break
    when 'station-switch'
      StationStore.setAudience []
      StationStore.setStation action.station
      StationStore.emitChange()
      break
    when 'station-listen'
      StationStore.setListening action.station
      StationStore.emitChange()
      break
    when "config-load"
      StationStore.loadConfig action.station,action.config
      StationStore.emitChange()
      break
    when "stations-load"
      StationStore.loadStations action.stations
      StationStore.emitChange()
      break
    when "stations-leave"
      StationStore.loadStations action.stations
      StationStore.unsetStation action.station
      StationStore.emitChange()
      break
    when "station-create"
      StationStore.createStation action.station
      StationStore.emitChange()
      break
    when "members-load"
      StationStore.loadMembers action.station,action.members
      StationStore.emitChange()
      break
    when "typing-set"
      StationStore.setTyping action.station,action.state
      StationStore.emitChange()
      break

module.exports = StationStore