StationActions = require '../actions/StationActions.coffee'

module.exports =
  createStation: (name,cb) ->
    window.urb.send {
      appl:"radio"
      mark:"radio-command"
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
      appl:"radio"
      mark:"radio-command"
      data:
        design: 
          party:name
          config:null
    }, cb

  setSources: (station,ship,sources) ->
    send = 
      appl:"radio"
      mark:"radio-command"
      data:
        design:
          party:station
          config:
            sources:sources
            caption:""
            cordon:{posture:"white", list:[]}
    window.urb.send send, (err,res) ->
      console.log 'add source updates'
      console.log arguments

  members: ->
    window.urb.subscribe {
      appl:"radio"
      path:"/a/court"
    }, (err,res) ->
      console.log 'membership updates'
      console.log res.data
      if res.data?.group?.global
        StationActions.loadMembers res.data.group.global

  listen: ->
    window.urb.subscribe {
      appl:"radio"
      path:"/"
     }, (err,res) ->
        console.log 'house updates'
        console.log res.data
        if res.data.house
          StationActions.loadStations res.data.house

  listenStation: (station) ->
    window.urb.subscribe {
      appl:"radio"
      path:"/ax/#{station}"
     }, (err,res) ->
        console.log('station subscription updates')
        console.log(res.data)
        if res.data.ok is true
          StationActions.listeningStation station
        if res.data.group?.global
          StationActions.loadMembers res.data.group.global
        if res.data.config
          StationActions.loadConfig station,res.data.config