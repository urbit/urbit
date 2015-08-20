recl = React.createClass
rece = React.createElement
[div,h1,label] = [React.DOM.div,React.DOM.h1,React.DOM.label]

module.exports = recl
  _onKeyDown: (e) -> 
    if e.keyCode is 13
      e.stopPropagation()
      e.preventDefault()
      @change(e)
  _onBlur: (e) -> @change(e)
  change: (e) ->
    $t = $(e.target).closest('.filter')
    txt = $t.find('.input').text().trim()
    key = $t.attr('data-key')
    if txt.length is 0 then txt = null
    if key is 'audience' then txt = txt.split " "
    if key is 'tags' then txt = [txt]
    @props.onChange key,txt
  render: ->
    (div {className:'filters'}, [
      (div {className:'owned filter ib','data-key':'owner'}, [
        (label {}, 'Owned by:')
        (div {
            contentEditable:true
            className:'input ib'
            onKeyDown:@_onKeyDown
            onBlur:@_onBlur
            },@props.filters.owned)
      ])
      (div {className:'tag filter ib','data-key':'tags'}, [
        (label {}, 'Tag:')
        (div {
            contentEditable:true
            className:'input ib'
            onKeyDown:@_onKeyDown
            onBlur:@_onBlur
            },@props.filters.tag)
      ])
      (div {className:'channel filter ib','data-key':'audience'}, [
        (label {}, 'Audience:')
        (div {
            contentEditable:true
            className:'input ib'
            onKeyDown:@_onKeyDown
            onBlur:@_onBlur
            },@props.filters.channel)
      ])
      (div {className:'status filter ib','data-key':'status'}, [
        (label {}, 'Status:')
        (div {
            contentEditable:true
            className:'input ib'
            onKeyDown:@_onKeyDown
            onBlur:@_onBlur
            },@props.filters.status)
      ])
    ])