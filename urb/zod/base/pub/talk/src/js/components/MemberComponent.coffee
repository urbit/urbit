recl = React.createClass
[div,input,textarea] = [React.DOM.div,React.DOM.input,React.DOM.textarea]

module.exports = recl
  render: ->
    if @props.ship[0] is "~" then @props.ship = @props.ship.slice(1)
    k = "ship"
    k+= " #{@props.presence}" if @props.presence
    div {className:"iden"}, [
      div {className:k}, @props.ship
    ]
