clas        = require 'classnames'

reactify    = (manx)-> React.createElement window.tree.reactify, {manx}
query       = require './Async.coffee'

recl = React.createClass
{div,a,ul,li,h1} = React.DOM

module.exports = query {
    path:'t'
    kids:
      snip:'r'
      head:'r'
      meta:'j'
  }, recl
  displayName: "List"
  
  render: ->
    k = clas
      list: true
      posts: @props.dataType is 'post'
      default: @props['data-source'] is 'default'
    (ul {className:k}, @renderList())

  renderList: ->
    # check if kids all have a sort meta tag
    sorted = true
    _keys = []
    for k,v of @props.kids
      if not v.meta?.sort? then sorted = false
      _keys[Number(v.meta?.sort)] = k
    if sorted isnt true
      _keys = _.keys(@props.kids).sort()
    if @props.dataType is 'post' then _keys=_keys.reverse()
    for item in _keys
      path = @props.path+"/"+item
      elem = @props.kids[item]
      href = window.tree.basepath path
      parts = []
      if elem.meta?.title
        title = 
          gn: 'h1'
          c: [elem.meta.title]
      else title = elem.head
      parts.push title
      if @props.dataPreview 
        if @props.dataType is 'post'
          parts.push (elem.snip.c.slice 0,2)...
        else
          parts.push elem.snip
      if @props.titlesOnly
        parts = elem.head
      li {key:item,className:@props.dataType ? ""},
        a {href,className:(clas preview: @props.dataPreview?)},            
          reactify
            gn: 'div'
            c: parts

          # if not @props.dataPreview? then (h1 {},item)
          # else if @props.dataType is 'post'
          #   head = 
          #     if elem.meta?.title
          #       gn: 'h1'
          #       c: [elem.meta.title]
          #     else elem.head
          #   reactify
          #     gn: 'div'
          #     c: [head, (elem.snip.c.slice 0,2)...]
          # else if @props.titlesOnly? then reactify elem.head
          # else div {}, (reactify elem.head), (reactify elem.snip)
