$ ->
  rend = React.render; rele = React.createElement
  rend (rele window.MessagesComponent, messages: window.MessageData), ($ '#cont')[0]
