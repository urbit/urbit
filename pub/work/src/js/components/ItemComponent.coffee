recl = React.createClass
rece = React.createElement
{div,textarea} = React.DOM

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
    WorkActions.setItem @props.item,'done',(not @props.item.done?)

  _changeStatus: (e) ->
    return if @props.item.status is 'released'
    if @props.item.status is 'accepted' and 
    @formatOwner(@props.item.owner) isnt window.urb.ship
      return
    own = "claim" if @props.item.status is "announced"
    own = "announce" if @props.item.status is "accepted"
    WorkActions.ownItem @props.item,own

  _submitComment: (e) ->
    $input = $(e.target).closest('.item').find('.comment .input')
    val = $input.text()
    return if val.length is 0
    WorkActions.addComment @props.item,val
    $input.text('')

  formatDate: (d=(new Date),l) ->
    _d = "~#{d.getFullYear()}.#{(d.getMonth()+1)}.#{d.getDate()}"
    if l
      _d += "..#{d.getHours()}.#{d.getMinutes()}.#{d.getSeconds()}"
    _d

  formatOwner: (o="") -> o.replace /\~/g,""

  formatAudience: (a=[]) -> @formatOwner a.join(" ")

  getInitialState: -> {expand:false}

  renderField: (_key,props,render=_.identity)->
    defaultValue =  @props.item[_key]
    rece Field, $.extend props, {render,_key,item:@props.item,defaultValue}
  
  renderTopField: (key,props,format)->
    _props = _.extend props,{className:"#{props.className ? key} top"}
    @renderField key,_props,format

  componentDidMount: ->
    setInterval =>
        $('.new.comment .date').text @formatDate()
      , 1000
  
  render: ->
    itemClass = 'item'
    if @state.expand then itemClass += ' expand'

    discussion = _.clone @props.item.discussion ? []
    discussion.reverse()

    action = ""
    if @props.item.status is 'announced'
      action = "claim"
    if @props.item.status is 'accepted' and @formatOwner(@props.item.owner) is window.urb.ship
      action = "release" 

    (div {
        className:itemClass
        draggable:true
        @onDragStart,@onDragEnd
      },
        (div {
          className:'header'
          },
            (div {className:'owner ib'}, @formatOwner(@props.item.owner))
            (div {
              className:'status ib action-'+(action.length > 0)
              'data-key':'status'
              onClick:@_changeStatus
              },
                (div {className:'label'}, @props.item.status)
                (div {className:'action a'}, action)
              )
            (@renderField 'audience', {}, @formatAudience)
          )
        (div {className:'sort ib top'}, @props.item.sort)
        (div {className:'done ib done-'+@props.item.done?, onClick:@_markDone}, '')
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
                (div {className:'date ib'}, @formatDate())
                (div {
                  contentEditable:true,
                  className:'input'})
                (div {className:'submit',onClick:@_submitComment},'Post')
            )
          )
    )
