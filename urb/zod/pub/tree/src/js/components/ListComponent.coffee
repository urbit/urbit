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
    _keys = _.keys(@props.kids).sort()
    if @props.dataType is 'post' then _keys=_keys.reverse()
    for item in _keys
      path = @props.path+"/"+item
      elem = @props.kids[item]
      href = window.tree.basepath path
      li {key:item,className:@props.dataType ? ""},
        a {href,className:(clas preview: @props.dataPreview?)},
          if not @props.dataPreview? then (h1 {},item)
          else if @props.dataType is 'post'
            head = 
              if elem.meta?.title
                gn: 'h1'
                c: [elem.meta.title]
              else elem.head
            reactify
              gn: 'div'
              c: [head, (elem.snip.c.slice 0,2)...]
          else if @props.titlesOnly? then reactify elem.head
          else div {}, (reactify elem.head), (reactify elem.snip)
