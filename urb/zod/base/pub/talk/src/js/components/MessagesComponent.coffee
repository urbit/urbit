moment = require 'moment-timezone'

recl = React.createClass
[div,br,input,textarea,a] = [React.DOM.div,React.DOM.br,React.DOM.input,React.DOM.textarea,React.DOM.a]

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
    audi = _.map $(e.target).closest('.audi').find('div'), (div) -> return "~"+$(div).text()
    @props._handleAudi audi

  _handlePm: (e) ->
    return if not @props._handlePm
    user = $(e.target).closest('.iden').text()
    return if user.toLowerCase() is 'system'
    @props._handlePm user

  render: ->
    # pendingClass = if @props.pending isnt "received" then "pending" else ""
    delivery = _.uniq _.pluck @props.thought.audience, "delivery"
    klass = if delivery.indexOf("received") isnt -1 then " received" else " pending"
    if @props.thought.statement.speech?.lin?.say is false then klass += " say"
    if @props.thought.statement.speech?.url then klass += " url"
    if @props.unseen is true then klass += " new"
    if @props.sameAs is true then klass += " same" else klass += " first"

    name = if @props.name then @props.name else ""
    aude = _.keys @props.thought.audience
    audi = window.util.clipAudi(aude).map (_audi) -> (div {}, _audi.slice(1))

    type = ['private','public']
    type = type[Number(aude.indexOf(window.util.mainStationPath(window.urb.user)) is -1)]

    if @props.thought.statement.speech?.lin?.txt then txt = @props.thought.statement.speech.lin.txt
    if @props.thought.statement.speech?.url 
      url = @props.thought.statement.speech.url.url
      txt = (a {href:url,target:"_blank"}, url)
    if @props.thought.statement.speech?.app
      txt = @props.thought.statement.speech.app.txt
      klass += " say"

    div {className:"message#{klass}"}, [
        (div {className:"attr"}, [
          div {className:"type #{type}"}, ""
          (div {onClick:@_handlePm}, (React.createElement Member,{ship:@props.ship}))
          div {onClick:@_handleAudi,className:"audi"}, audi
          div {className:"time"}, @convTime @props.thought.statement.date
        ])
        div {className:"mess"}, txt
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

  _blur: ->
    @focussed = false
    @lastSeen = @last

  _focus: ->
    @focussed = true
    @lastSeen = null
    $('.message.new').removeClass 'new'
    document.title = document.title.replace /\ \([0-9]*\)/, ""

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

  sortedMessages: (messages) ->
    _.sortBy messages, (_message) -> 
          _message.pending = _message.thought.audience[station]
          _message.thought.statement.date

  componentDidMount: ->
    MessageStore.addChangeListener @_onChangeStore
    StationStore.addChangeListener @_onChangeStore
    if @state.station and
    @state.listening.indexOf(@state.station) is -1
      MessageActions.listenStation @state.station
    checkMore = @checkMore
    $(window).on 'scroll', checkMore
    @focussed = true
    $(window).on 'blur', @_blur
    $(window).on 'focus', @_focus
    window.util.setScroll()

  componentDidUpdate: ->
    $window = $(window)
    if @lastLength
      st = $window.height()
      $window.scrollTop st
      @lastLength = null
    else
      if not window.util.isScrolling()
        window.util.setScroll()
      else
        console.log 'scrolling'

    if @focussed is false and @last isnt @lastSeen
      _messages = @sortedMessages @state.messages
      d = _messages.length-_messages.indexOf(@lastSeen)-1
      t = document.title
      if document.title.match(/\([0-9]*\)/)
        document.title = document.title.replace /\([0-9]*\)/, "(#{d})"
      else
        document.title = document.title + " (#{d})" 
      
  componentWillUnmount: ->
    MessageStore.removeChangeListener @_onChangeStore
    StationStore.removeChangeListener @_onChangeStore

  _onChangeStore: -> @setState @stateFromStore()

  _handlePm: (user) ->
    audi = [window.util.mainStationPath(user)]
    if user is window.urb.user then audi.pop()
    StationActions.setAudience audi

  _handleAudi: (audi) -> StationActions.setAudience audi

  render: ->
    station = @state.station
    _station = "~"+window.urb.ship+"/"+station
    sources = _.clone @state.configs[@state.station]?.sources ? []
    sources.push _station
    _messages = @sortedMessages @state.messages

    @last = _messages[_messages.length-1]
    @length = _messages.length

    setTimeout => 
        @checkMore() if length < @pageSize
      , 1

    lastIndex = if @lastSeen then _messages.indexOf(@lastSeen) else null
    lastSaid = null

    messages = _messages.map (_message,k) => 
      if lastIndex and lastIndex is k then _message.unseen = true
      if _message.thought.statement.speech?.app
        _message.ship = "system"
      _message.sameAs = lastSaid is _message.ship
      _message.station = @state.station
      _message._handlePm = @_handlePm
      _message._handleAudi = @_handleAudi
      lastSaid = _message.ship
      React.createElement Message,_message
    div {id: "messages"}, messages
