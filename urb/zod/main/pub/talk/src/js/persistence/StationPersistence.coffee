StationActions = require '../actions/StationActions.coffee'

module.exports =
  createStation: (name,cb) ->
    window.urb.send {
      appl:"talk"
      mark:"talk-command"
      data:
        design: 
          party:name
          config:
            sources:[]
            caption:""
            cordon:{posture:"white", list:[]}
    }, cb

  removeStation: (name,cb) ->
    window.urb.send {
      appl:"talk"
      mark:"talk-command"
      data:
        design: 
          party:name
          config:null
    }, cb

  setSources: (station,ship,sources) ->
    send = 
      appl:"talk"
      mark:"talk-command"
      data:
        design:
          party:station
          config:
            sources:sources
            caption:""
            cordon:{posture:"white", list:[]}
    window.urb.send send, (err,res) ->
      console.log 'talk-command'
      console.log arguments

  members: ->
    window.urb.subscribe {
      appl:"talk"
      path:"/a/court"
    }, (err,res) ->
      if err or not res
        console.log '/a/ err'
        console.log err
        return
      console.log '/a/'
      console.log res.data
      if res.data?.group?.global
        StationActions.loadMembers res.data.group.global

  listen: ->
    window.urb.subscribe {
      appl:"talk"
      path:"/"
     }, (err,res) ->
        if err or not res.data
          console.log '/ err'
          console.log err
          return
        console.log '/'
        console.log res.data
        if res.data.house
          StationActions.loadStations res.data.house

  listenStation: (station) ->
    window.urb.subscribe {
      appl:"talk"
      path:"/ax/#{station}"
     }, (err,res) ->
        if err or not res
          console.log '/ax/ err'
          console.log err
          return
        console.log('/ax/')
        console.log(res.data)
        if res.data.ok is true
          StationActions.listeningStation station
        if res.data.group
          res.data.group.global[window.util.mainStationPath(window.urb.user)] = res.data.group.local
          StationActions.loadMembers res.data.group.global
        if res.data.cabal?.loc
          StationActions.loadConfig station,res.data.cabal.loc