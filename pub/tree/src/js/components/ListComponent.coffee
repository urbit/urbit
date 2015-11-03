clas        = require 'classnames'

reactify    = require './Reactify.coffee'
query       = require './Async.coffee'

recl = React.createClass
{div,pre,span,a,ul,li,h1} = React.DOM

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
      @props.dataType
      posts: @props.dataType is 'post' # needs css update
      default: @props['data-source'] is 'default'
    kids = @renderList()
    unless kids.length is 0 and @props.is404?
      return (ul {className:k}, kids)

    div {className:k},
      h1  {className:'error'}, 'Error: Empty path'
      div {},
        pre  {}, @props.path
        span {}, 'is either empty or does not exist.'

  renderList: ->
    # check if kids all have a sort meta tag
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
    if sorted isnt true
      _keys = _.keys(@props.kids).sort()
    if @props.dataType is 'post' then _keys=_keys.reverse()
    for item in _.values _keys
      path = @props.path+"/"+item
      elem = @props.kids[item]
      if elem.meta.hide? then continue
      href = window.tree.basepath path
      if elem.meta.link then href = elem.meta.link
      parts = []
      title = null
      if elem.meta?.title
        title = 
          gn: 'h1'
          c: [elem.meta.title]
      if not title && elem.head.c.length > 0
        title = elem.head
      if not title
        title =
          gn: 'h1'
          c: [item]
      unless @props.titlesOnly        # redundant? this seems familiar
        if @props.dataDates
          _date = elem.meta.date
          if not _date or _date.length is 0 then _date = ""
          date = 
            gn: 'div'
            ga: 
              className: 'date'
            c: [_date]
          parts.push date
      parts.push title
      unless @props.titlesOnly        # redundant? this seems familiar
        if @props.dataPreview 
          if @props.dataType is 'post' and not elem.meta.preview
            parts.push (elem.snip.c.slice 0,2)...
          else
            if elem.meta.preview 
              preview = 
                gn: 'p'
                c: [elem.meta.preview]
            else 
              preview = elem.snip
            parts.push preview
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
