recl = React.createClass
[div,input,textarea] = [React.DOM.div,React.DOM.input,React.DOM.textarea]

module.exports = recl
  render: ->
    if @props.ship[0] isnt "~" then @props.ship = "~"+@props.ship
    k = "ship"
    k+= " #{@props.presence}" if @props.presence
    div {className:"iden"}, [
      div {className:k}, @props.ship
    ]
