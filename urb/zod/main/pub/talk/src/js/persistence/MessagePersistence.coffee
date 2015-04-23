MessageActions = require '../actions/MessageActions.coffee'

module.exports =
  listenStation: (station,since) ->
    console.log 'listen station'
    console.log arguments
    $this = this
    window.urb.subscribe {
      appl:"talk"
      path:"/f/#{station}/#{since}"
     }, (err,res) ->
        if err or not res.data
          console.log '/f/ err!'
          console.log err
          console.log res
          $this.listenStation station,since
          return
        console.log('/f/')
        console.log(res.data)
        if res.data.ok is true
          MessageActions.listeningStation station
        if res.data?.grams?.tele
          MessageActions.loadMessages res.data.grams

  get: (station,start,end) ->
    end   = window.urb.util.numDot end
    start = window.urb.util.numDot start
    window.urb.subscribe {
      appl:"talk"
      path:"/f/#{station}/#{end}/#{start}"
    }, (err,res) ->
      if err or not res.data
        console.log '/f/ /e/s err'
        console.log err
        return
      console.log '/f/ /e/s'
      console.log res        
      if res.data?.grams?.tele
        MessageActions.loadMessages res.data.grams,true
        window.urb.unsubscribe {
          appl:"talk"
          path:"/f/#{station}/#{end}/#{start}"
        }, (err,res) ->
          console.log 'done'
          console.log res

  sendMessage: (message,cb) ->
    window.urb.send {
      appl:"talk"
      mark:"talk-command"
      data:
        publish: [
          message
        ]
    }, (err,res) ->
      console.log 'sent'
      console.log arguments
      cb(err,res) if cb