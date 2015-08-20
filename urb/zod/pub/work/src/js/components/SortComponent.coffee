recl = React.createClass
rece = React.createElement
[div,h1,button] = [React.DOM.div,React.DOM.h1,React.DOM.button]

module.exports = recl
  render: ->
    (div {className:'sorts'}, [
      (h1 {}, 'Sorts:')
      (button {}, 'Name')
      (button {}, 'Owner')
      (button {}, 'Date')
      (button {}, 'Priority')
    ])