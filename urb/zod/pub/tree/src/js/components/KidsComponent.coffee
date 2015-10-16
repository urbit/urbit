reactify    = require './Reactify.coffee'
query       = require './Async.coffee'

recl = React.createClass
{div,a,ul,li,hr} = React.DOM

module.exports = query {kids: {body:'r', meta:'j'}}, recl
  displayName: "Kids"
  render: -> 
    klass = "kids"
    if @props.dataType then klass += " #{@props.dataType}"

    sorted = true
    _keys = []
    for k,v of @props.kids
      if @props.sortBy
        if @props.sortBy is 'date'
          if not v.meta?.date? then sorted = false
          _k = Number v.meta.date.slice(1).replace /\./g,""
          _keys[_k] = k
      else
        if not v.meta?.sort? then sorted = false
        _keys[Number(v.meta?.sort)] = k
    if @props.sortBy is 'date' then _keys.reverse()
    if sorted isnt true then _keys = _.keys(@props.kids).sort()

    div {className:klass},
      for item in _.values _keys
        elem = @props.kids[item]
        [(div {key:item}, reactify elem.body), (hr {},"")]
