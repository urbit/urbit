recl = React.createClass
{div,pre,br,input,textarea,a} = React.DOM


Message = recl
  lz: (n) -> if n<10 then "0#{n}" else "#{n}"

  convTime: (time) ->
    d = new Date time
    h = @lz d.getHours()
    m = @lz d.getMinutes()
    s = @lz d.getSeconds()
    "~#{h}.#{m}.#{s}"

  render: ->
    # pendingClass = if @props.pending isnt "received" then "pending" else ""
    delivery = _.uniq _.pluck @props.thought.audience, "delivery"
    klass = if delivery.indexOf("received") isnt -1 then " received" else " pending"
    speech = @props.thought.statement.speech
    attachments = []
    while speech.fat?
      attachments.push pre {}, speech.fat.tor.tank.txt.join("\n")
      speech = speech.fat.taf  # XX
    if !speech? then return;
    if speech.lin?.say is false then klass += " say"
    if speech.url then klass += " url"

    name = if @props.name then @props.name else ""
    aude = _.keys @props.thought.audience
    audi = window.util.clipAudi(aude).map (_audi) -> (div {}, _audi.slice(1))

    type = ['private','public']
    type = type[Number(aude.indexOf(window.util.mainStationPath(window.urb.ship)) is -1)]

    mess = switch
      when (con = speech.lin) or (con = speech.app) or
           (con = speech.exp) or (con = speech.tax)
        con.txt
      when (con = speech.url)
        (a {href:con.txt,target:"_blank"}, con.txt)
      else "Unknown speech type:" + (" %"+x for x of speech).join ''
    
    klass += switch
      when speech.app? then " say"
      when speech.exp? then " exp"
      else ""
    
    {ship} = @props
    if ship[0] is "~" then ship = ship.slice(1)
    
    div {className:"message#{klass}"}, [
        (div {className:"attr"}, [
          div {className:"type #{type}"}, ""
          div {className:"iden"}, div {className:"ship"}, ship
          div {className:"audi"}, audi
          div {className:"time"}, @convTime @props.thought.statement.date
        ])
        div {className:"mess"}, mess,
          if attachments.length
            div {className:"fat"}, attachments
      ]

window.MessagesComponent = recl
  pageSize: 50
  paddingTop: 100

  getInitialState: -> {station:window.location.pathname.split("/").reverse()[0]}

  sortedMessages: (messages) ->
    _.sortBy messages, (_message) -> 
          _message.pending = false
          _message.thought.statement.date

  render: ->
    _messages = @sortedMessages @props.messages

    messages = _messages.map (_message,k) => 
      if _message.thought.statement.speech?.app
        _message.ship = "system"
      _message.station = @state.station
      React.createElement Message,_message
    div {id: "messages"}, messages
