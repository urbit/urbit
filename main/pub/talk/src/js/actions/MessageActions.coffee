MessageDispatcher = require '../dispatcher/Dispatcher.coffee'

# hm

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

  sendMessage: (station,message,audience) ->
    serial = window.util.uuid32()

    if station[0] isnt "~" then station = "~"+window.urb.ship+"/"+station

    if audience.length is 0 then audience.push station

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
              say:false
              txt:message
          date: Date.now()

    MessageDispatcher.handleViewAction
      type:"message-send"
      message:_message
    window.chat.MessagePersistence.sendMessage _message.thought