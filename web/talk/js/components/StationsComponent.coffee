recl = React.createClass
{div,input} = React.DOM

StationStore = require '../stores/StationStore.coffee'
StationActions = require '../actions/StationActions.coffee'

module.exports = recl
  stateFromStore: -> {
    stations: StationStore.getStations()
    station: "~zod/court"
  }

  getInitialState: -> @stateFromStore()

  componentDidMount: ->
    @$el = $ @getDOMNode()
    @$add = $ '#stations .add'
    @$input = @$el.find 'input'
    StationStore.addChangeListener @_onChangeStore
      
  componentWillUnmount: ->
    StationStore.removeChangeListener @_onChangeStore

  _onChangeStore: -> @setState @stateFromStore()

  _click: (e) ->
    s = $(e.target).closest('.station').find('.name').text()
    window.location.hash = "/#{s.toLowerCase()}"

  _keyUp: (e) ->
    if e.keyCode is 13
      v = @$input.val().toLowerCase()
      if @state.stations.indexOf(v) is -1
        StationActions.createStation v
        @$input.val('')
        @$input.blur()

  _remove: (e) ->
    _station = $(e.target).parent().find('.name').text()
    _stations = _.without @state.stations,_station
    StationActions.removeStation _station,_stations
    e.stopPropagation()
    e.preventDefault()

  render: ->
    station = @state.station
    _click = @_click
    _remove = @_remove
    stations = @state.stations.map (_station) ->
      k = "station"
      parts = [(div {className:"name"}, _station.name)]
      if _station.name isnt window.util.mainStation()
        parts.push (div {className:"remove",onClick:_remove,dataStation:_station.name},"×")
      div {className:k,onClick:_click},parts

    div {id:"stations"}, [
      div {className:"stations"},stations
      div {className:"join-ctrl"}, [
        input {className:"join",onKeyUp:@_keyUp,placeholder:"+"}, ""
      ]
    ]