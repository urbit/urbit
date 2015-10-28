MessageDispatcher = require '../dispatcher/Dispatcher.coffee'

module.exports =
  loadMessages: (grams,get) ->
    MessageDispatcher.handleServerAction
      type:"messages-load"
      messages:grams.tele
      last:grams.num
      get:get

  listenStation: (station,date) ->
    if not date then date = window.urb.util.toDate(new Date())
    window.talk.MessagePersistence.listenStation station,date

  listeningStation: (station) ->
    MessageDispatcher.handleViewAction
      type:"messages-listen"
      station:station

  setTyping: (state) ->
    MessageDispatcher.handleViewAction
      type:"messages-typing"
      state:state    

  getMore: (station,start,end) ->
    MessageDispatcher.handleViewAction
      type:"messages-fetch"
    window.talk.MessagePersistence.get station,start,end

  sendMessage: (message,audience) ->
    serial = window.util.uuid32()

    # audience.push window.util.mainStationPath window.urb.user
    audience = _.uniq audience

    _audi = {}
    for k,v of audience
      _audi[v] = 
        envelope:
          visible:true
          sender:null
        delivery:"pending"

    speech = 
      lin:
        say:true
        txt:message

    if message[0] is "@"
      speech.lin.txt = speech.lin.txt.slice(1).trim()
      speech.lin.say = false
      
    else if message[0] is "#"
      speech = eval: speech.lin.txt.slice(1).trim()

    else if window.urb.util.isURL(message)
      speech = url: message

    speeches =
      if not (speech.lin?.txt.length > 64)
        [speech]
      else 
        {say,txt} = speech.lin
        txt.match(/(.{1,64}$|.{0,64} |.{64}|.+$)/g).map (s)->
          lin: {say, txt:
            if s.slice -1 isnt " "
              s
            else s.slice 0,-1
          }
        
    for speech in speeches
      _message =
        ship:window.urb.ship
        thought:
          serial:window.util.uuid32()
          audience:_audi
          statement:
            bouquet:[]
            speech:speech
            date: Date.now()

      MessageDispatcher.handleViewAction
        type:"message-send"
        message:_message
      window.talk.MessagePersistence.sendMessage _message.thought

