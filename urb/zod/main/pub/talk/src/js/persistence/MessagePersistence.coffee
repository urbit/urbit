MessageActions = require '../actions/MessageActions.coffee'

module.exports =
  listenStation: (station,since) ->
    window.urb.subscribe {
      appl:"rodeo"
      path:"/f/#{station}/#{since}"
     }, (err,res) ->
        console.log('m subscription updates')
        console.log(res.data)
        if res.data.ok is true
          MessageActions.listeningStation station
        if res.data?.grams?.tele
          MessageActions.loadMessages res.data.grams

  get: (station,start,end) ->
    window.urb.subscribe {
      appl:"rodeo"
      path:"/f/#{station}/#{end}/#{start}"
    }, (err,res) ->
      console.log 'get'
      console.log res
      if res.data?.grams?.tele
        MessageActions.loadMessages res.data.grams,true
        window.urb.unsubscribe {
          appl:"rodeo"
          path:"/f/#{station}/#{end}/#{start}"
        }, (err,res) ->
          console.log 'done'
          console.log res

  sendMessage: (message,cb) ->
    window.urb.send {
      appl:"rodeo"
      mark:"rodeo-command"
      data:
        publish: [
          message
        ]
    }, (err,res) ->
      console.log 'sent'
      console.log arguments
      cb(err,res) if cb