moment = require 'moment-timezone'

EventEmitter = require('events').EventEmitter

MessageDispatcher = require '../dispatcher/Dispatcher.coffee'

_messages = {}
_fetching = false
_last = null
_station  = null
_listening = []
_typing = false

MessageStore = _.merge new EventEmitter,{
  removeChangeListener: (cb) -> @removeListener "change", cb

  emitChange: -> @emit 'change'

  addChangeListener: (cb) -> @on 'change', cb

  leadingZero: (str) ->
    if Number(str) < 10 then "0"+str else str

  convertDate: (time) ->
    time = time.substr(1).split(".")
    time[1] = @leadingZero time[1]
    time[2] = @leadingZero time[2]
    d = new moment "#{time[0]}-#{time[1]}-#{time[2]}T#{time[4]}:#{time[5]}:#{time[6]}Z"
    d.tz "Europe/London"
    d

  getListening: -> _listening

  getTyping: -> _typing

  getLastAudience: ->
    if _.keys(_messages).length is 0 then return []
    messages = _.sortBy _messages, (_message) -> _message.thought.statement.time
    _.keys messages[messages.length-1].thought.audience

  setTyping: (state) -> _typing = state

  setListening: (station) -> 
    if _listening.indexOf(station) isnt -1
      console.log 'already listening on that station (somehow).'
    else
      _listening.push station

  setStation: (station) -> _station = station

  sendMessage: (message) ->
    _messages[message.thought.serial] = message

  loadMessages: (messages,last,get) ->
    key = last
    for v in messages
      serial = v.thought.serial
      v.key = key++
      # always overwrite with new
      _messages[serial] = v
    _last = last if last < _last or _last is null or get is true
    _fetching = false

  getAll: -> _.values _messages

  getFetching: -> _fetching

  setFetching: (state) -> _fetching = state

  getLast: -> _last
}

MessageStore.setMaxListeners 100

MessageStore.dispatchToken = MessageDispatcher.register (payload) ->
  action = payload.action
  
  switch action.type
    when 'station-switch'
      MessageStore.setStation action.station
      break
    when 'messages-listen'
      MessageStore.setListening action.station
      MessageStore.emitChange()
      break
    when 'messages-typing'
      MessageStore.setTyping action.state
      MessageStore.emitChange()
      break
    when 'messages-fetch'
      MessageStore.setFetching true
      MessageStore.emitChange()
      break
    when 'messages-load'
      MessageStore.loadMessages action.messages,action.last,action.get
      MessageStore.emitChange()
      break
    when 'message-load'
      MessageStore.loadMessage action.time,action.message,action.author
      MessageStore.emitChange()
      break
    when 'message-send'
      MessageStore.sendMessage action.message
      MessageStore.emitChange()
      break

module.exports = MessageStore
