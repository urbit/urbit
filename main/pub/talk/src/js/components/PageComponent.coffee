recl = React.createClass
[div,input,textarea] = [React.DOM.div,React.DOM.input,React.DOM.textarea]

StationComponent    = require './StationComponent.coffee'
MessagesComponent   = require './MessagesComponent.coffee'
WritingComponent    = require './WritingComponent.coffee'

module.exports = recl
  render: ->
    div {id:"d"}, "asdf"
    # div {id:"d"}, [
    #   (div {id:'station-container'}, (StationComponent {}, ""))
    #   (div {id:'messages-container'}, (MessagesComponent {}, ""))
    #   (div {id:'writing-container'}, (WritingComponent {}, ""))
    # ]