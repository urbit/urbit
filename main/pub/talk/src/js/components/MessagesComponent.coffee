moment = require 'moment-timezone'

recl = React.createClass
[div,br,input,textarea] = [React.DOM.div,React.DOM.br,React.DOM.input,React.DOM.textarea]

MessageActions  = require '../actions/MessageActions.coffee'
MessageStore    = require '../stores/MessageStore.coffee'
StationActions  = require '../actions/StationActions.coffee'
StationStore    = require '../stores/StationStore.coffee'
Member          = require './MemberComponent.coffee'

Message = recl
  lz: (n) -> if n<10 then "0#{n}" else "#{n}"

  convTime: (time) ->
    d = new Date time
    h = @lz d.getHours()
    m = @lz d.getMinutes()
    s = @lz d.getSeconds()
    "~#{h}.#{m}.#{s}"

  _handleAudi: (e) ->
    audi = $(e.target).closest('.audi').text().split(" ")
    @props._handleAudi audi

  _handlePm: (e) ->
    return if not @props._handlePm
    user = $(e.target).closest('.iden').text().slice(1)
    @props._handlePm user

  render: ->
    # pendingClass = if @props.pending isnt "received" then "pending" else ""
    delivery = _.uniq _.pluck @props.thought.audience, "delivery"
    pendingClass = if delivery.indexOf("received") isnt -1 then "received" else "pending"

    name = if @props.name then @props.name else ""
    audi = window.util.clipAudi _.keys @props.thought.audience
    audi = audi.join " "

    div {className:"message "+pendingClass}, [
        (div {className:"attr"}, [
          div {onClick:@_handleAudi,className:"audi"}, "#{audi}"
          (div {onClick:@_handlePm}, (Member {ship:@props.ship}, ""))
          (br {},"")
          div {className:"time"}, @convTime @props.thought.statement.date
        ])
        div {className:"mess"}, @props.thought.statement.speech.lin.txt
      ]

module.exports = recl
  pageSize: 50
  paddingTop: 100

  stateFromStore: -> {
    messages:MessageStore.getAll()
    last:MessageStore.getLast()
    fetching:MessageStore.getFetching()
    listening:MessageStore.getListening()
    station:window.util.mainStation()
    stations:StationStore.getStations()
    configs:StationStore.getConfigs()
    typing:MessageStore.getTyping()
  }

  getInitialState: -> @stateFromStore()

  checkMore: ->
    if $(window).scrollTop() < @paddingTop &&
    @state.fetching is false &&
    this.state.last &&
    this.state.last > 0
      end = @state.last-@pageSize
      end = 0 if end < 0
      @lastLength = @length
      MessageActions.getMore @state.station,(@state.last+1),end

  setAudience: -> 
    return if not @last
    if _.keys(@last.thought.audience).length > 0 and @state.typing is false and
    _.difference(_.keys(@last.thought.audience),@state.audi).length is 0
      StationActions.setAudience _.keys(@last.thought.audience)

  componentDidMount: ->
    MessageStore.addChangeListener @_onChangeStore
    StationStore.addChangeListener @_onChangeStore
    if @state.station and @state.listening.indexOf(@state.station) is -1
      MessageActions.listenStation @state.station
    checkMore = @checkMore
    $(window).on 'scroll', checkMore
    window.util.setScroll()

  componentDidUpdate: ->
    $window = $(window)
    if @lastLength
      h = $('.message').height() * (@length-@lastLength)
      st = $window.scrollTop()
      $window.scrollTop st+h
      @lastLength = null
    else
      if $('#writing-container').length > 0
        window.util.setScroll()
      
  componentWillUnmount: ->
    MessageStore.removeChangeListener @_onChangeStore
    StationStore.removeChangeListener @_onChangeStore

  _onChangeStore: -> @setState @stateFromStore()

  _handlePm: (user) ->
    audi = [
      window.util.mainStationPath(user)
      window.util.mainStationPath(window.urb.user)
    ]
    StationActions.setAudience audi

  _handleAudi: (audi) -> StationActions.setAudience audi

  render: ->
    station = @state.station
    _station = "~"+window.urb.ship+"/"+station
    sources = _.clone @state.configs[@state.station]?.sources ? []
    sources.push _station
    _messages = @state.messages
    # _messages = _.filter @state.messages, (_message) ->
    #   audience = _.keys(_message.thought.audience)
    #   _.intersection(sources,audience).length > 0
    _messages = _.sortBy _messages, (_message) -> 
      _message.pending = _message.thought.audience[station]
      _message.thought.statement.time

    @last = _messages[_messages.length-1]
    @length = _messages.length

    setTimeout => 
        @checkMore() if length < @pageSize
      , 1

    messages = _messages.map (_message) => 
      _message.station = @state.station
      _message._handlePm = @_handlePm
      _message._handleAudi = @_handleAudi
      Message _message, ""
    div {id: "messages"}, messages