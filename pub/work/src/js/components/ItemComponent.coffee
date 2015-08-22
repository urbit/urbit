recl = React.createClass
{div,textarea} = React.DOM

WorkActions   = require '../actions/WorkActions.coffee'

module.exports = recl
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

  getVal: ($el,key) ->
    if $el[0].tagName is 'TEXTAREA'
      return $el.val()
    else
      if key is 'date_due'
        d = $el.text().slice(1).replace(/\./g, "-")
        return NaN if d.length < 8
        return new Date(d).valueOf()      
      if key is 'tags'
        return $el.text().trim().split(" ")
      if key is 'audience'
        a = $el.text().trim().split(" ")
        a = a.map (_a) -> "~#{_a}"
        return a
      return $el.text()

  compareVal: (l,n,key) ->
    if key is 'tags' or key is 'audience'
      return (_.xor(l,n).length > 0)
    if key is 'date_due'
      return l isnt new Date(n)
    l isnt n

  validateField: ($t,key,val) ->
    valid = 1
    if key is 'date_due'
      valid = 0 if isNaN(val)
    if key is 'audience'
      i = _.filter val,(a) -> 
        if a[0] isnt "~"
          return 0
        if a.split("/").length < 2
          return 0
        if a.split("/")[0].length < 3 or
        a.split("/")[1].length < 3
          return 0
        1
      valid = 0 if i.length isnt val.length
    valid

  onKeyUp: (e) ->
    $t = $(e.target).closest '.field'
    key = $t.attr 'data-key'
    val = @getVal $t.find('.input'),key

    if @compareVal @props.item[key],val,key
      if not @validateField($t,key,val) 
        $t.addClass 'invalid'
        return
      $t.removeClass 'invalid'
      if @to then clearTimeout @to
      @to = setTimeout => 
          WorkActions.setItem @props.item,key,val
        ,1000

  onFocus: (e) -> @props._focus e,@

  _markDone: (e) -> WorkActions.setItem @props.item,'done',true

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

  formatDate: (d,l) ->
    return "" if d is null
    _d = "~#{d.getFullYear()}.#{(d.getMonth()+1)}.#{d.getDate()}"
    if l
      _d += "..#{d.getHours()}.#{d.getMinutes()}.#{d.getSeconds()}"
    _d

  formatOwner: (o) ->
    return "" if o is null
    o.replace /\~/g,""

  formatAudience: (a) ->
    @formatOwner a.join(" ")

  getInitialState: -> {expand:false}

  renderField: (key,props,format=_.identity)->
    _props = _.extend {}, props, {contentEditable:true,className:'input ib'}
    className = "#{props.className ? key} field ib"
    (div {className,'data-key':key}, (div _props, format(@props.item[key])))
  
  renderTopField: (key,props,format)->
    _props = _.extend {className:"#{props.className ? key} top"}, props
    @renderField key,_props,format

  componentDidMount: ->
    formatDate = @formatDate
    setInterval ->
        $('.new.comment .date').text formatDate (new Date()),true
      , 1000
  
  render: ->
    itemClass = 'item'
    if @state.expand then itemClass += ' expand'

    discussion = _.clone @props.item.discussion
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
      }, [
        (div {
          className:'header'
          },[
            (div {className:'owner ib'}, @formatOwner(@props.item.owner))
            (div {
              className:'status ib action-'+(action.length > 0)
              'data-key':'status'
              onClick:@_changeStatus
              },[
                (div {className:'label'}, @props.item.status)
                (div {className:'action a'}, action)
              ])
            (@renderField 'audience', {@onKeyUp}, @formatAudience) # no onKeyUp?
          ])
        (div {className:'sort ib top'}, @props.item.sort)
        (div {className:'done ib done-'+@props.item.done?, onClick:@_markDone}, '')
        (@renderTopField 'title', {@onFocus,@onKeyDown,@onKeyUp})
        (@renderField 'date_due', {@onKeyUp,className:'date top'}, @formatDate)
        (@renderTopField 'tags', {@onKeyUp}, (tags)-> tags.join(" "))
        (div {
          className:'expand ib',
          onClick: (e) => @setState {expand:!@state.expand}
          }, (div {className:'caret left'},"")
        )
        (@renderField 'description',{@onKeyUp})
      
        (div {className:"hr"},"")
        (div {className:"discussion"},[
          (div {className:"comments"}, discussion.map (slug) =>
              (div {className:'comment'}, [
                (div {className:'hr2'},"")
                (div {className:'ship ib'}, slug.ship)
                (div {className:'date ib'}, @formatDate(slug.date,true))
                (div {className:'body'}, slug.body)
              ])
          ),
          (div {className:'new comment'},[
              (div {className:'hr2'},"")
              (div {className:'ship ib'}, window.urb.ship)
              (div {className:'date ib'}, @formatDate(new Date(),true))
              (div {
                contentEditable:true,
                className:'input'},"")
              (div {className:'submit',onClick:@_submitComment},'Post')
          ])
        ])
    ])
