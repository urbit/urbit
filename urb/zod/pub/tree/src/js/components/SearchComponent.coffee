query       = require './Async.coffee'
reactify    = require './Reactify.coffee'

recl = React.createClass
{div,input} = React.DOM


module.exports = query {kids: sect:'r'}, recl
  hash:null
  displayName: "Search"
  getInitialState: -> search: 'dva'
  onKeyUp: (e)-> @setState search: e.target.value
  render: -> div {},
    input {@onKeyUp,ref:'inp',defaultValue:'dva'}
    _(c for x,{sect:{c}} of @props.kids)
      .flatten()
      .map(@highlight)
      .filter()
      .map(reactify)
      .value()
    
  highlight: (e)->
    return e unless @state.search
    got = false
    res = reactify.walk e,
      ()-> null
      (s)=>
        m = s.split @state.search
        return [s] unless m[1]?
        lit = gn:'span',c:[@state.search],ga:style:background: '#ff6'
        got = true
        [m[0], _.flatten([lit,s] for s in m[1..])...]
      ({gn,ga,c})->{gn,ga,c:_.flatten c}
    res if got
