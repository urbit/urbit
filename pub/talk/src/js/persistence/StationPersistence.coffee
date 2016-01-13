window.urb.appl = "talk"
send = (data,cb)-> window.urb.send data, {mark:"talk-command"}, cb
design = (party,config,cb)-> send {design:{party,config}}, cb
module.exports = ({StationActions})->
  createStation: (name,cb) ->
    design name, {
      sources:[]
      caption:""
      cordon: posture:"white", list:[]
    }, cb

  removeStation: (name,cb) -> design name, null, cb
  setSources: (station,ship,sources) ->
    cordon = posture:"white", list:[]
    design station, {sources,cordon,caption:""}, (err,res) ->
      console.log 'talk-command'
      console.log arguments

  members: -> window.urb.bind "/a/court", (err,res) ->
    if err or not res
      console.log '/a/ err'
      console.log err
      return
    console.log '/a/'
    console.log res.data
    if res.data?.group?.global
      StationActions.loadMembers res.data.group.global

  listen: -> window.urb.bind "/", (err,res) ->
    if err or not res.data
      console.log '/ err'
      console.log err
      return
    console.log '/'
    console.log res.data
    if res.data.house
      StationActions.loadStations res.data.house

  listenStation: (station) -> window.urb.bind "/avx/#{station}", (err,res) ->
    if err or not res
      console.log '/avx/ err'
      console.log err
      return
    console.log('/avx/')
    console.log(res.data)
    if res.data.ok is true
      StationActions.listeningStation station
    if res.data.group
      res.data.group.global[window.util.mainStationPath(window.urb.user)] =
        res.data.group.local
      StationActions.loadMembers res.data.group.global
    if res.data.cabal?.loc
      StationActions.loadConfig station,res.data.cabal.loc
    if res.data.glyph
      StationActions.loadGlyphs res.data.glyph
