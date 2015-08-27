recl = React.createClass
rece = React.createElement
{div,h1,label} = React.DOM

module.exports = recl
  onClick: (e) ->
    switch @props.filters['done']
      when null
        b = true
      when true
        b = false
      when false
        b = null
    @props.onChange 'done',b
  onKeyDown: (e) -> 
    if e.keyCode is 13
      e.stopPropagation()
      e.preventDefault()
      @change(e)
  onBlur: (e) -> @change(e)
  change: (e) ->
    $t = $(e.target).closest('.filter')
    txt = $t.find('.input').text().trim()
    key = $t.attr('data-key')
    if txt.length is 0 then txt = null
    else switch key
      when 'creator'    then txt = "~#{txt}"
      when 'audience' then txt = txt.split " "
      when 'tags'     then txt = [txt]
    @props.onChange key,txt
  
  fields: [ {filter:'done',    key:'done',    title: ''},
            {filter:'owned',   key:'creator',    title: 'Owner:'},
            {filter:'tag',     key:'tags',     title: 'Tag:'},
            {filter:'channel', key:'audience', title: 'Audience:'},
            {filter:'status',  key:'status',   title: 'Status:'} ]

  render: ->
    (div {className:'filters'}, @fields.map ({filter,key,title})=>
      (div {key, 'data-key':key, className:"#{filter} filter ib"},
        (label {}, title)
        switch filter
          when 'done'
            (div {
                  className:'input-bool ib '+@props.filters[key],
                  @onClick
                },"")
          else
            (div {
                contentEditable:true
                className:'input ib'
                @onKeyDown
                @onBlur
              },@props.filters[filter])
      ))
