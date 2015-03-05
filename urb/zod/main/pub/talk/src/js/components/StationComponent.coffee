recl = React.createClass
[div,input,textarea,h1,a] = [
  React.DOM.div,
  React.DOM.input,
  React.DOM.textarea,
  React.DOM.h1,
  React.DOM.a
]

StationStore    = require '../stores/StationStore.coffee'
StationActions  = require '../actions/StationActions.coffee'
Member          = require './MemberComponent.coffee'

module.exports = recl
  stateFromStore: -> {
    audi:StationStore.getAudience()
    members:StationStore.getMembers()
    station:StationStore.getStation()
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

  _toggleAudi: (e) ->
    $e = $(e.target).closest('.station')
    station = $e.find('.path').text()
    StationActions.toggleAudience station

  _onChangeStore: -> 
    @setState @stateFromStore()

  _keyUp: (e) ->
    if e.keyCode is 13
      v = @$input.val()
      if @state.configs[@state.station].sources.indexOf(v) is -1
        _sources = _.clone @state.configs[@state.station].sources
        _sources.push v
        StationActions.setSources @state.station,_sources
        @$input.val('')

  _remove: (e) ->
    e.stopPropagation()
    e.preventDefault()
    _station = $(e.target).attr "data-station"
    _sources = _.clone @state.configs[@state.station].sources
    _sources.slice _sources.indexOf(_station),1
    StationActions.setSources @state.station,_sources

  render: ->
    parts = []
    members = []

    if @state.station and @state.members[@state.station]
      members = _.map @state.members[@state.station], (state,member) -> 
          Member {ship:member,presence:state.presence}
    else
      members = ""

    sourceInput = [(input {className:"join",onKeyUp:@_keyUp,placeholder:"+"}, "")]
    sourceCtrl = div {className:"sour-ctrl"}, sourceInput

    sources = []
    if @state.station and @state.configs[@state.station]
      _remove = @_remove
      _sources = _.clone @state.configs[@state.station].sources
      _sources.push "twitter/hoontap"
      sources = _.map _sources,(source) =>
        toggleClass = "toggle "
        if @state.audi.indexOf(source) isnt -1 then toggleClass += "active"
        (div {className:"station",onClick:@_toggleAudi}, [
          (div {className:toggleClass})
          (div {className:"path"}, source),
          (div {className:"remove",onClick:_remove,"data-station":source},"×")
        ])
        
    else
      sources = ""

    station = []
    station.push (a {className:"up",href:"\#/"}, [(div {className:"arow-up"}, "")])
    station.push (h1 {},@state.station)
    station.push (div {id:"members"},members)

    parts.push (div {id:"station-container"}, (div {id:"station-meta"},station))
    parts.push (div {id:"sources-container"}, [(div {class:"sources-list"},sources),sourceCtrl])

    div {id:"station"},parts