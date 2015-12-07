recl = React.createClass
rece = React.createElement
{div,textarea,button} = React.DOM

WorkActions   = require '../actions/WorkActions.coffee'
Field         = require './FieldComponent.coffee'

module.exports = recl
  displayName: 'Item'
  onDragStart: (e) ->
    unless @props.draggable
      e.preventDefault()
      return
    $t = $(e.target)
    @dragged = $t.closest('.item')
    e.dataTransfer.effectAllowed = 'move'
    e.dataTransfer.setData 'text/html',e.currentTarget
    @props._dragStart e,@
  
  onDragEnd: (e) -> @props._dragEnd e,@

  onKeyDown: (e) -> 
    @props.title_keyDown e,@

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

  onFocus: (e) -> @props._focus e,@

  _markDone: (e) -> 
    WorkActions.setItem @props.item,'done',(not (@props.item.done is true))

  getStatus: -> switch @props.item.doer
    when window.urb.ship then "owned"
    when undefined      then ""
    when null then "available"
    else  "taken: ~"+@props.item.doer

  getAction: -> switch @props.item.doer
    when null
      action = "claim"
    when window.urb.ship
      action = "release" 
    else ""

  _changeStatus: (e) ->
    return if @props.item.status is 'released'
    if @props.item.status is 'accepted' and 
    @formatCreator(@props.item.creator) isnt window.urb.ship
      return
    WorkActions.ownItem @props.item,@getAction()

  _submitComment: (e) ->
    $input = $(e.target).closest('.item').find('.comment .input')
    val = $input.text()
    return if val.length is 0
    WorkActions.addComment @props.item,val
    $input.text('')

  formatDate: (d,l) ->
    unless d
      return ""
    _d = "~#{d.getFullYear()}.#{(d.getMonth()+1)}.#{d.getDate()}"
    if l
      _d += "..#{d.getHours()}.#{d.getMinutes()}.#{d.getSeconds()}"
    _d

  formatCreator: (o="") -> o.replace /\~/g,""

  formatAudience: (a=[]) -> @formatCreator a.join(" ")

  getInitialState: -> {expand:false}

  renderField: (_key,props,render=_.identity)->
    {item,index} =  @props
    defaultValue =  item[_key]
    rece Field, $.extend props, {render,_key,defaultValue,item,index}
  
  renderTopField: (key,props,format)->
    _props = _.extend props,{className:"#{props.className ? key} top"}
    @renderField key,_props,format

  componentDidMount: ->
    setInterval =>
        $('.new.comment .date').text @formatDate new Date
      , 1000
  
  render: ->
    itemClass = 'item'
    if @state.expand then itemClass += ' expand'

    discussion = _.clone @props.item.discussion ? []
    discussion.reverse()

    status = @getStatus()
    action = @getAction()
    
    (div {
        className:itemClass
        draggable:true
        @onDragStart,@onDragEnd
      },
        (div {
          className:'header'
          },
            (div {className:'creator ib'}, @formatCreator(@props.item.creator))
            (div {
              className:'status ib action-'+(action.length > 0)
              'data-key':'status'
              onClick:@_changeStatus
              },
                (div {className:'label'}, status)
                (div {className:'action a'}, action)
              )
            (@renderField 'audience', {}, @formatAudience)
          )
        (div {className:'sort ib'}, '')
        (button {className:'done ib done-'+(@props.item.done is true), onClick:@_markDone}, '')
        (@renderTopField 'title', {@onFocus,@onKeyDown})
        (@renderTopField 'date_due', {className:'date'}, @formatDate)
        (@renderTopField 'tags', {}, (tags=[])-> tags.join(" "))
        (div {
          className:'expand ib',
          onClick: (e) => @setState {expand:!@state.expand}
          }, (div {className:'caret left'},"")
        )
        (@renderField 'description',textarea: yes)
      
        (div {className:"hr"},"")
        if discussion?
          (div {className:"discussion"},
            (div {className:"comments"}, discussion.map (slug) =>
                (div {className:'comment',key:slug.date},
                  (div {className:'hr2'},"")
                  (div {className:'ship ib'}, slug.ship)
                  (div {className:'date ib'}, @formatDate slug.date,true)
                  (div {className:'body'}, slug.body)
                )
            ),
            (div {className:'new comment'},
                (div {className:'hr2'},"")
                (div {className:'ship ib'}, window.urb.ship)
                (div {className:'date ib'}, @formatDate new Date)
                (div {
                  contentEditable:true,
                  className:'input'})
                (button {className:'submit',onClick:@_submitComment},'Post')
            )
          )
    )
