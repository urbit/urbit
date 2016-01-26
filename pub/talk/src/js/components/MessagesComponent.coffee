moment = require 'moment-timezone'
clas = require 'classnames'

recl = React.createClass
{div,pre,br,span,input,textarea,a} = React.DOM

MessageActions  = require '../actions/MessageActions.coffee'
MessageStore    = require '../stores/MessageStore.coffee'
StationActions  = require '../actions/StationActions.coffee'
StationStore    = require '../stores/StationStore.coffee'
Member          = require './MemberComponent.coffee'

Message = recl
  displayName: "Message"
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

  renderSpeech: (speech)-> switch
    when (con = speech.lin) or (con = speech.app) or
         (con = speech.exp) or (con = speech.tax)
      con.txt
    when (con = speech.url)
      (a {href:con.txt,target:"_blank"}, con.txt)
    when (con = speech.mor) then con.map @renderSpeech
    else "Unknown speech type:" + (" %"+x for x of speech).join ''

  render: ->
    # pendingClass = clas pending: @props.pending isnt "received"
    delivery = _.uniq _.pluck @props.thought.audience, "delivery"
    speech = @props.thought.statement.speech
    attachments = []
    while speech.fat?
      attachments.push pre {}, speech.fat.tor.tank.join("\n")
      speech = speech.fat.taf  # XX
    if !speech? then return;
    
    name = if @props.name then @props.name else ""
    aude = _.keys @props.thought.audience
    audi = window.util.clipAudi(aude).map (_audi) -> (div {}, _audi.slice(1))

    mainStation = window.util.mainStationPath(window.urb.user)
    type = if mainStation in aude then 'private' else 'public'

    className = clas {message:true},
      (if @props.sameAs then "same" else "first"),
      (if delivery.indexOf("received") isnt -1 then "received" else "pending"),
      {say: speech.lin?.say is false, url: speech.url, 'new': @props.unseen},
      switch
        when speech.app? then "say"
        when speech.exp? then "exp"
        
    div {className}, [
        (div {className:"attr"}, [
          div {className:"type #{type}", "data-glyph": @props.glyph || "*"}
          (div {onClick:@_handlePm},
           (React.createElement Member,{ship:@props.ship,glyph:@props.glyph}))
          div {onClick:@_handleAudi,className:"audi"}, audi
          div {className:"time"}, @convTime @props.thought.statement.date
        ])
        
        div {className:"mess"}, 
          (@renderSpeech speech)
          if attachments.length
            div {className:"fat"}, attachments
      ]

module.exports = recl
  displayName: "Messages"
  pageSize: 50
  paddingTop: 100

  stateFromStore: -> {
    messages:MessageStore.getAll()
    last:MessageStore.getLast()
    fetching:MessageStore.getFetching()
    listening:MessageStore.getListening()
    station:window.talk.mainStation
    stations:StationStore.getStations()
    configs:StationStore.getConfigs()
    typing:MessageStore.getTyping()
    glyphs:StationStore.getGlyphMap()
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
          _message.key
          #_message.thought.statement.date

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
    if @last?.ship && @last.ship is window.urb.user then @lastSeen = @last
    @length = _messages.length

    setTimeout => 
        @checkMore() if length < @pageSize
      , 1

    lastIndex = if @lastSeen then _messages.indexOf(@lastSeen)+1 else null
    lastSaid = null

    div {id: "messages"}, _messages.map (_message,k) =>
      nowSaid = [_message.ship,_message.thought.audience]
      glyph = window.util.getGlyph @state.glyphs, _.keys _message.thought.audience
      {station} = @state
      mess = {
        glyph, station, @_handlePm, @_handleAudi,
        unseen: lastIndex and lastIndex is k
        sameAs: _.isEqual lastSaid, nowSaid
      }
      lastSaid = nowSaid
              
      if _message.thought.statement.speech?.app
        mess.ship = "system"

      React.createElement Message, (_.extend {}, _message, mess)
