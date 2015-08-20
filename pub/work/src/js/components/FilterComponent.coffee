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
    if txt.length is 0 then txt = null
    @props.onChange $t.attr('data-key'),txt
  render: ->
    (div {className:'filters'}, [
      (h1 {}, 'Filters:')
      (div {className:'owned filter','data-key':'owned'}, [
        (label {}, 'Owened by:')
        (div {
            contentEditable:true
            className:'input ib'
            onKeyDown:@_onKeyDown
            onBlur:@_onBlur
            },@props.filters.owned)
      ])
      (div {className:'tag filter','data-key':'tag'}, [
        (label {}, 'Tag:')
        (div {
            contentEditable:true
            className:'input ib'
            },@props.filters.tag)
      ])
      (div {className:'channel filter','data-key':'channel'}, [
        (label {}, 'Channel:')
        (div {
            contentEditable:true
            className:'input ib'
            },@props.filters.channel)
      ])
      (div {className:'status filter','data-key':'status'}, [
        (label {}, 'Status:')
        (div {
            contentEditable:true
            className:'input ib'
            },@props.filters.status)
      ])
    ])