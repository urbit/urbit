recl = React.createClass
rece = React.createElement
{div,textarea} = React.DOM

WorkActions   = require '../actions/WorkActions.coffee'

cediv = recl render: ->
  div _.extend {}, @props,
    contentEditable: true
    dangerouslySetInnerHTML: __html: $('<div>').text(@props.text).html()
    
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
        (div {className:'sort ib top'},@props.item.sort)
        (div {className:'done ib'},'')
        (div {className:'title ib top'},[
          (rece cediv, {
            onFocus:@_focus
            onKeyDown:@_keyDown
            className:'input ib'
            text: @props.item.title
          }) 
        ])
        (div {className:'date ib top'}, [
          (rece cediv, {
            className:'input ib'
            text: @formatDate(@props.item['date-created'])
          }) 
        ])
        (div {className:'tags ib top'},[
          (rece cediv, {
            className:'input ib'
            text: @props.item.tags.join(" ")
          }) 
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
            className:'input ib'
            },@props.item.description)
        ])
        (div {className:"hr"},"")
        (div {className:"discussion"},[
          (div {className:"comments"}, @props.item.discussion.map (slug) =>
              (div {className:'comment'}, [
                (div {className:'hr2'},"")
                (div {className:'ship ib'}, slug.ship)
                (div {className:'date ib'}, @formatDate(slug.date))
                (div {className:'body'}, slug.body)
              ])
          ),
          (div {className:'new comment'},[
              (div {className:'hr2'},"")
              (div {className:'ship ib'}, "TALSUR-TODRES")
              (div {className:'date ib'}, @formatDate(new Date()))
              (div {
                contentEditable:true,
                className:'input'},"")
              (div {className:'submit'},'Post')
          ])
        ])
    ])
