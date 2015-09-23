reactify    = require './Reactify.coffee'

recl = React.createClass
{div,p,button,input} = React.DOM

module.exports = recl
  displayName: "email"

  getInitialState: -> {submit:false,email:""}

  onClick: -> @submit()
  onKeyUp: (e) -> 
    $('div.email').text($('div.email').text())
    if e.keyCode is 13 then @submit()

  submit: ->
    email = $('div.email').text()
    @setState {submit:true}

  render: ->
    if @state.submit is false
      cont = [
        (input {key:"field",className:"email",placeholder:"your@email.com",@onKeyUp}, @state.email)
        (button {key:"submit",className:"submit",@onClick}, "Submit")
      ]
    else
      cont = [(div {className:"submitted"},"Got it. Thanks!")]
    (p {className:"email"}, cont)