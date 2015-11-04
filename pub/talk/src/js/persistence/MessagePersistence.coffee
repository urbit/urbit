window.urb.appl = "talk"
send = (data,cb)-> window.urb.send data, {mark:"talk-command"}, cb

module.exports = ({MessageActions}) ->
  listenStation: (station,since) ->
    console.log 'listen station'
    console.log arguments
    $this = this
    window.urb.bind "/f/#{station}/#{since}", (err,res) ->
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
          {tele,num} = res.data?.grams
          MessageActions.loadMessages tele, num

  get: (station,start,end) ->
    end   = window.urb.util.numDot end
    start = window.urb.util.numDot start
    window.urb.bind "/f/#{station}/#{end}/#{start}", (err,res) ->
      if err or not res.data
        console.log '/f/ /e/s err'
        console.log err
        return
      console.log '/f/ /e/s'
      console.log res        
      if res.data?.grams?.tele
        {tele,num} = res.data?.grams
        MessageActions.loadMessages tele,num,true
        window.urb.drop "/f/#{station}/#{end}/#{start}", (err,res) ->
          console.log 'done'
          console.log res

  sendMessage: (message,cb) ->
    send {publish: [message]}, (err,res) ->
      console.log 'sent'
      console.log arguments
      cb(err,res) if cb
