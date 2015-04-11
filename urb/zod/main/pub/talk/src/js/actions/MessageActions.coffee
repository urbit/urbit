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
    window.chat.MessagePersistence.listenStation station,date

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
    window.chat.MessagePersistence.get station,start,end

  sendMessage: (message,audience) ->
    serial = window.util.uuid32()

    audience.push window.util.mainStationPath window.urb.user
    audience = _.uniq audience

    _audi = {}
    for k,v of audience
      _audi[v] = 
        envelope:
          visible:true
          sender:null
        delivery:"pending"

    _message =
      ship:window.urb.ship
      thought:
        serial:serial
        audience:_audi
        statement:
          bouquet:[]
          speech:
            lin:
              say:true
              txt:message
          date: Date.now()
    
    if message[0] is "@"
      _message.thought.statement.speech.lin.txt = _message.thought.statement.speech.lin.txt.slice(1).trim()
      _message.thought.statement.speech.lin.say = false

    if window.urb.util.isURL(message)
      _message.thought.statement.speech = {url: message}

    MessageDispatcher.handleViewAction
      type:"message-send"
      message:_message
    window.chat.MessagePersistence.sendMessage _message.thought