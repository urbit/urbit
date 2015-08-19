recl = React.createClass
[div,textarea] = [React.DOM.div,React.DOM.textarea]

WorkActions   = require '../actions/WorkActions.coffee'

module.exports = recl
  _dragStart: (e) ->
    $t = $(e.target)
    @dragged = $t.closest('.item')
    e.dataTransfer.effectAllowed = 'move'
    e.dataTransfer.setData 'text/html',e.currentTarget
    @props._dragStart e,@
  
  _dragEnd: (e) -> @props._dragEnd e,@

  _keyDown: (e) -> 
    @props._keyDown e,@

    kc = e.keyCode

    switch kc
      # tab - expand
      when 9
        if @state.expand is false
          @setState {expand:true}
      # esc - collapse
      when 27
        @setState {expand:false}

    if (kc is 9 and @state.expand is false) or (kc is 27) 
      e.preventDefault()
      return

  _focus: (e) -> @props._focus e,@

  formatDate: (d) ->
    "#{d.getDate()}-#{(d.getMonth()+1)}-#{d.getFullYear()}"

  getInitialState: -> {expand:false}

  render: ->
    itemClass = 'item'
    if @state.expand then itemClass += ' expand'

    (div {
      className:itemClass
      draggable:true
      'data-index':@props.index
      onDragStart:@_dragStart
      onDragEnd:@_dragEnd
      'data-index':@props.index
      }, [
        (div {className:'audience'},@props.item.audience.join(" "))
        (div {className:'sort ib top'},@props.index)
        (div {className:'done ib'},'')
        (div {className:'title ib top'},[
          (div {
            contentEditable:true
            onFocus:@_focus
            onKeyDown:@_keyDown
            className:'input'
          },@props.item.title)
        ])
        (div {className:'date ib top'}, [
          (div {
            contentEditable:true
            className:'input'
            },@formatDate(@props.item['date-created']))
        ])
        (div {className:'tags ib top'},[
          (div {
            contentEditable:true
            className:'input'
            },@props.item.tags.join(" "))
        ])
        (div {
          className:'expand ib',
          onClick: (e) =>
            @setState {expand:!@state.expand}
          },[
          (div {className:'caret left'},"")
        ])
        (div {className:"description"},[
          (textarea {
            className:'input'
            },@props.item.description)
        ])
        (div {className:"hr"},"")
        (div {className:"discussion"},[
          (div {className:"comments"}, @props.item.discussion.map (slug) ->
            (div {className:'slug'}, slug)
          ),
          (div {
            contentEditable:true
            className:'input comment'
            },"")
        ])
    ])