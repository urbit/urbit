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

  getVal: ($el,key) ->
    if $el[0].tagName is 'TEXTAREA'
      return $el.val()
    else
      if key is 'date-due'
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
    if key is 'date-due'
      return l isnt new Date(n)
    l isnt n

  validateField: ($t,id,key,val) ->
    valid = 1
    if key is 'date-due'
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

  _keyUp: (e) ->
    $t = $(e.target).closest '.field'
    id = $t.closest('.item').attr 'data-id'
    key = $t.attr 'data-key'
    val = @getVal $t.find('.input'),key

    if @compareVal @props.item[key],val,key
      if not @validateField($t,id,key,val) 
        $t.addClass 'invalid'
        return
      $t.removeClass 'invalid'
      if @to then clearTimeout @to
      ver = @props.item.version
      @to = setTimeout -> 
          WorkActions.setItem id,ver,key,val
        ,1000

  _focus: (e) -> @props._focus e,@

  _markDone: (e) ->
    id = $(e.target).closest('.item').attr 'data-id'
    WorkActions.setItem id,@props.item.version,'done',true

  _claim: (e) ->

  _release: (e) ->

  _submitComment: (e) ->
    $t = $(e.target).closest('.item')
    id = $t.attr 'data-id'
    val = $t.find('.comment .input').text()

    WorkActions.addComment id,@props.item.version,val

  formatDate: (d) ->
    return "" if d is null
    "~#{d.getFullYear()}.#{(d.getMonth()+1)}.#{d.getDate()}"

  formatOwner: (o) ->
    return "" if o is null
    o.replace /\~/g,""

  formatAudience: (a) ->
    @formatOwner a.join(" ")

  getInitialState: -> {expand:false}

  render: ->
    itemClass = 'item'
    if @state.expand then itemClass += ' expand'

    owner = []
    if @props.item.owner?.slice(1) is window.urb.ship
      k = 'mine'
      owner.push (div {className:'release a',onClick:@_release},'Release')
    if @props.item.owner is null
      k = 'open'
      owner.push (div {className:'claim a',onClick:@_claim}, "Claim")
    owner.unshift (div {className:k},@formatOwner(@props.item.owner))

    (div {
      className:itemClass
      draggable:true
      'data-id':@props.item.id
      'data-index':@props.index
      onDragStart:@_dragStart
      onDragEnd:@_dragEnd
      }, [
        (div {
          className:'owner ib'
          'data-key':'owner'
          },owner)
        (div {
          className:'audience field ib'
          'data-key':'audience'
          },[
          (div {
            contentEditable:true
            className:'input ib'
            },@formatAudience(@props.item.audience))
          ])
        (div {className:'sort ib top'},@props.item.sort)
        (div {
          className:'done ib'
          onClick:@_markDone
          },'')
        (div {
          className:'title ib top field'
          'data-key':'title'
          },[
          (div {
            contentEditable:true
            onFocus:@_focus
            onKeyDown:@_keyDown
            onKeyUp:@_keyUp
            className:'input ib'
          },@props.item.title)
        ])
        (div {
          className:'date ib top field'
          'data-key':'date-due'
          }, [
          (div {
            contentEditable:true
            className:'input ib'
            onKeyUp:@_keyUp
            },@formatDate(@props.item['date-due']))
        ])
        (div {
          className:'tags ib top field'
          'data-key':'tags'
          },[
          (div {
            contentEditable:true
            className:'input ib'
            onKeyUp:@_keyUp
            },@props.item.tags.join(" "))
        ])
        (div {
          className:'expand ib',
          onClick: (e) =>
            @setState {expand:!@state.expand}
          },[
          (div {className:'caret left'},"")
        ])
        (div {
          className:'description field'
          'data-key':'description'
          },[
          (textarea {
            className:'input ib'
            onKeyUp:@_keyUp
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
              (div {className:'ship ib'}, window.urb.ship)
              (div {className:'date ib'}, @formatDate(new Date()))
              (div {
                contentEditable:true,
                className:'input'},"")
              (div {className:'submit',onClick:@_submitComment},'Post')
          ])
        ])
    ])