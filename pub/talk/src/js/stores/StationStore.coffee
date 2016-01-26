EventEmitter      = require('events').EventEmitter

StationDispatcher = require '../dispatcher/Dispatcher.coffee'

_audience   = []
_members    = {}
_stations   = []
_listening  = []
_station    = null
_config     = {}
_typing     = {}
_glyphs     = {}
_shpylg     = {}

_validAudience = true

StationStore = _.merge new EventEmitter,{
  removeChangeListener: (cb) -> @removeListener "change", cb

  emitChange: -> @emit 'change'

  addChangeListener: (cb) -> @on 'change', cb

  getAudience: -> _audience

  setAudience: (audience) -> _audience = audience

  getValidAudience: -> _validAudience

  setValidAudience: (valid) -> _validAudience = valid

  toggleAudience: (station) ->
    if _audience.indexOf(station) isnt -1
      _audience.splice _audience.indexOf(station), 1
    else
      _audience.push station

  loadConfig: (station,config) -> _config[station] = config

  getConfigs: -> _config

  getConfig: (station) -> _config[station]
  
  getGlyph: (station) -> _shpylg[station]
  
  getGlyphMap: -> _shpylg
  
  getGlyphAudience: (glyph) ->
    aud = _glyphs[glyph] ? []
    if aud.length is 1
      aud[0]

  getMember: (ship) -> {ship:ship}

  loadMembers: (members) -> 
    _members = {}
    for station,list of members
      for member,presence of list
        _members[member] = {} if not _members[member]
        _members[member][station] = presence

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
  
  loadGlyphs: (glyphs) ->
    _glyphs = glyphs
    _shpylg = {}
    for char,auds of glyphs
      for aud in auds
        _shpylg[aud.join " "] = char

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
    when 'station-set-valid-audience'
      StationStore.setValidAudience action.valid
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
    when "config-load" #[name:'loadConfig', args:['station', 'config']]
      StationStore.loadConfig action.station,action.config
      StationStore.emitChange()
      break
    when "glyphs-load" #[name:'loadConfig', args:['station', 'config']]
      StationStore.loadGlyphs action.glyphs
      StationStore.emitChange()
      break
    when "stations-load"
      StationStore.loadStations action.stations
      StationStore.emitChange()
      break
    when "stations-leave"  # stations-leave:[{name:'loadStations' args:['stations']} ['unsetStation' 'station']]
                           # ...
                           # for command in actionVtable[action.type]
                           #   StationStore[command[0]].apply(command[1..].map(argname -> action[argname]))
      StationStore.loadStations action.stations
      StationStore.unsetStation action.station
      StationStore.emitChange()
      break
    when "station-create"
      StationStore.createStation action.station
      StationStore.emitChange()
      break
    when "members-load"
      StationStore.loadMembers action.members
      StationStore.emitChange()
      break
    when "typing-set"
      StationStore.setTyping action.station,action.state
      StationStore.emitChange()
      break

module.exports = StationStore
