recl = React.createClass
{div,input,textarea} = React.DOM

module.exports = recl
  render: ->
    if @props.ship[0] is "~" then @props.ship = @props.ship.slice(1)
    k = "ship"
    k+= " #{@props.presence}" if @props.presence
    div {className:"iden"}, [
      # div {}, @props.glyph || "*"
      div {className:k}, @props.ship
    ]
