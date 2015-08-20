recl = React.createClass
rece = React.createElement
[div,h1,input,textarea] = [React.DOM.div,React.DOM.h1,React.DOM.input,React.DOM.textarea]

module.exports = recl
  render: ->
    (div {className:'listening'}, [
      (h1 {}, 'Listening:')
    ])