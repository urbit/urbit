Dispatcher   =  require '../dispatcher/Dispatcher.coffee'

_persistence = require '../persistence/MessagePersistence.coffee'
  
Persistence = _persistence MessageActions: module.exports = 
  loadMessages: (messages,last,get) ->
    Dispatcher.handleServerAction {messages,last,get,type:"messages-load"}

  listenStation: (station,date) ->
    if not date then date = window.urb.util.toDate(new Date())
    Persistence.listenStation station,date

  listeningStation: (station) ->
    Dispatcher.handleViewAction {station,type:"messages-listen"}

  setTyping: (state) ->
    Dispatcher.handleViewAction {state,type:"messages-typing"}

  getMore: (station,start,end) ->
    Dispatcher.handleViewAction type:"messages-fetch"      
    Persistence.get station,start,end

  sendMessage: (txt,audience) ->
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

    speech = lin: {txt, say:true}

    if txt[0] is "@"
      speech.lin.txt = speech.lin.txt.slice(1).trim()
      speech.lin.say = false
      
    else if txt[0] is "#"
      speech = eval: speech.lin.txt.slice(1).trim()

    else if window.urb.util.isURL(txt)
      speech = url: txt

    speeches =
      if not (speech.lin?.txt.length > 64)
        [speech]
      else 
        {say,txt} = speech.lin
        txt.match(/(.{1,64}$|.{0,64} |.{64}|.+$)/g).map (s,i)->
          say ||= i isnt 0
          lin: {say, txt:
            if s.slice -1 isnt " "
              s
            else s.slice 0,-1
          }
        
    for speech in speeches
      message =
        ship:window.urb.ship
        thought:
          serial:window.util.uuid32()
          audience:_audi
          statement:
            bouquet:[]
            speech:speech
            date: Date.now()

      Dispatcher.handleViewAction {message,type:"message-send"}
      Persistence.sendMessage message.thought

