recl = React.createClass
rece = React.createElement
{div,h1,button,label} = React.DOM

module.exports = recl
  onClick: (e) ->
    $t = $(e.target).closest '.sort'
    key = $t.attr('data-key')
    sor = Number $t.attr 'data-state'
    if sor is 0 then sor = 1 
    else if sor is 1 then sor = -1
    else if sor is -1 then sor = 0
    @props.onChange key,sor

  render: ->
    (div {className:'sorts'}, _.map @props.sorts,(state,key) =>
      (button {
        key
        @onClick
        'data-key':key
        'data-state':state
        className:"sort s-#{state}"
        },
          (label {}, key)
          (div {className:'caret ib'}, '')
        )
    )
