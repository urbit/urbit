recl = React.createClass
{div,style,input,textarea,h1,a} = React.DOM

StationStore    = require '../stores/StationStore.coffee'
StationActions  = require '../actions/StationActions.coffee'
Member          = require './MemberComponent.coffee'

module.exports = recl
  displayName: "Station"
  stateFromStore: -> {
    audi:StationStore.getAudience()
    members:StationStore.getMembers()
    station:window.talk.mainStation
    stations:StationStore.getStations()
    configs:StationStore.getConfigs()
    typing:StationStore.getTyping()
    listening:StationStore.getListening()
  }

  getInitialState: -> @stateFromStore()

  componentDidMount: ->
    @$el = $(@getDOMNode())
    @$input = @$el.find('input')

    StationStore.addChangeListener @_onChangeStore
    if @state.listening.indexOf(@state.station) is -1
      StationActions.listenStation @state.station
      
  componentWillUnmount: ->
    StationStore.removeChangeListener @_onChangeStore


  _onChangeStore: -> 
    @setState @stateFromStore()

  _toggleOpen: (e) ->
    if $(e.target).closest('.sour-ctrl').length > 0
      return
    $("#station-container").toggleClass 'open'

  validateSource: (s) ->
    if @state.configs[@state.station].sources.indexOf(s) isnt -1
      return false
    if s.length < 5
      return false
    if s[0] isnt "~"
      return false
    if s.indexOf("/") is -1
      return false
    return true

  _keyUp: (e) ->
    $('.sour-ctrl .join').removeClass 'valid-false'
    if e.keyCode is 13
      v = @$input.val().toLowerCase()
      if v[0] isnt "~" then v = "~#{v}"
      if @validateSource v
        _sources = _.clone @state.configs[@state.station].sources
        _sources.push v
        StationActions.setSources @state.station,_sources
        @$input.val('')
        @$input.blur()
      else
        $('.sour-ctrl .join').addClass 'valid-false'

  _remove: (e) ->
    e.stopPropagation()
    e.preventDefault()
    _station = $(e.target).attr "data-station"
    _sources = _.clone @state.configs[@state.station].sources
    _sources.splice _sources.indexOf(_station),1
    StationActions.setSources @state.station,_sources

  render: ->
    if window.urb.user isnt window.urb.ship #foreign
      return div {id:"station"}
    
    parts = []
    members = []

    if @state.station and @state.members
      members = _.map @state.members, (stations,member) -> 
          audi = _.map stations,(presence,station) -> (div {className:"audi"}, station.slice(1))
          (div {}, [(React.createElement Member, {ship:member}),audi])
    else
      members = ""

    sourceInput = [(input {className:"join",onKeyUp:@_keyUp,placeholder:"+"})]
    sourceCtrl = div {className:"sour-ctrl"},sourceInput

    sources = []
    if @state.station and @state.configs[@state.station]
      _remove = @_remove
      _sources = _.clone @state.configs[@state.station].sources
      sources = _.map _sources,(source) =>
        (div {className:"station"}, [
          (div {className:"path"}, source.slice(1))
          (div {className:"remove",onClick:_remove,"data-station":source},"×"),
        ])
    else
      sources = "" 

    (div {id:"station",onClick:@_toggleOpen},
      (div {id:"head"}, 
        (div {id:"who"},
          div {className:"sig"}
          div {className:"ship"},"#{window.urb.user}"
        )
        (div {id:"where"},
          div {className:"slat"},"talk"
          div {className:"path"} #, window.util.mainStation(window.urb.user))
          div {className:"caret"}
        )
        div {id:"offline"}, "Warning: no connection to server."
      )
      div {id:"stations"}, (h1 {}, "Listening to"),(div {},sources),sourceCtrl
      div {id:"audience"}, div {}, (h1 {}, "Talking to"),(div {id:"members"},members)
    )
