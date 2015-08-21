recl = React.createClass
rece = React.createElement
{div,h1,label} = React.DOM

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
    else switch key
      when 'owner'    then txt = "~#{txt}"
      when 'audience' then txt = txt.split " "
      when 'tags'     then txt = [txt]
    @props.onChange key,txt
  
  fields: [ {filter:'owned',   key:'owner',    title: 'Owner:'},
            {filter:'tag',     key:'tags',     title: 'Tag:'},
            {filter:'channel', key:'audience', title: 'Audience:'},
            {filter:'status',  key:'status',   title: 'Status:'} ]

  render: ->
    (div {className:'filters'}, @fields.map ({filter,key,title})=>
      (div {key, 'data-key':key, className:"#{filter} filter ib"}, [
        (label {}, title)
        (div {
            contentEditable:true
            className:'input ib'
            onKeyDown:@_onKeyDown
            onBlur:@_onBlur
            },@props.filters[filter])
      ]))
