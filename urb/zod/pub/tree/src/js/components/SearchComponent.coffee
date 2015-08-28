query       = require './Async.coffee'
reactify    = require './Reactify.coffee'

recl = React.createClass
{a,div,input} = React.DOM


module.exports = query {name:'t', kids: sect:'j'}, recl
  hash:null
  displayName: "Search"
  getInitialState: -> search: 'wut'
  onKeyUp: (e)-> @setState search: e.target.value
  wrap: (elem,dir,path)->
    path = path[...-1] if path[-1...] is "/"
    href = @props.name+"/"+dir+path
    if elem?.ga?.id
      {gn,ga,c} = elem
      ga = _.clone ga
      href += "#"+ga.id
      delete ga.id
      elem = {gn,ga,c}
    {gn:'div', c:[{gn:'a', ga:{href}, c:[elem]}]}
    
  render: -> div {},
    input {@onKeyUp,ref:'inp',defaultValue:'wut'}
    _(@props.kids)
      .map(({sect},dir)=> @wrap h,dir,path for h in heds for path,heds of sect)
      .flatten()
      .flatten()
      .map(@highlight)
      .filter()
      .take(50)
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
