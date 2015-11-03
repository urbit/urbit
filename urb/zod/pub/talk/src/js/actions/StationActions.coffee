Dispatcher   =  require '../dispatcher/Dispatcher.coffee'
serverAction = (f)-> ()-> Dispatcher.handleServerAction f.apply this,arguments
viewAction   = (f)-> ()-> Dispatcher.handleViewAction   f.apply this,arguments

_persistence = require '../persistence/StationPersistence.coffee'
      
Persistence = _persistence StationActions: module.exports = 
  loadGlyphs:          serverAction (glyphs) -> {glyphs,type:"glyphs-load"}      
  loadMembers:        serverAction (members) -> {members,type:"members-load"}      
  loadStations:      serverAction (stations) -> {stations,type:"stations-load"}
  loadConfig:  serverAction (station,config) -> {station,config,type:"config-load"}

  setTyping:  viewAction (station,state) -> {station,state,type:"typing-set"}
  setAudience:     viewAction (audience) -> {audience,type:"station-set-audience"}
  setValidAudience:   viewAction (valid) -> {valid,type:"station-set-valid-audience"}
  toggleAudience:   viewAction (station) -> {station,type:"station-audience-toggle"}    
  switchStation:    viewAction (station) -> {station,type:"station-switch"}
  listeningStation: viewAction (station) -> {station,type:"station-listen"}

  createStation: (station) ->
    Dispatcher.handleViewAction {station,type: "station-create"}
    Persistence.createStation station
    
  listen:    () -> Persistence.listen()
  ping: (_ping) -> Persistence.ping _ping
  removeStation: (station) -> Persistence.removeStation station
  listenStation: (station) -> Persistence.listenStation station
  createStation:    (name) -> Persistence.createStation name
  
  setSources: (station,sources) ->
    Persistence.setSources station,window.urb.ship,sources
