recl = React.createClass
[div,input,textarea] = [React.DOM.div,React.DOM.input,React.DOM.textarea]

MessageActions  = require '../actions/MessageActions.coffee'
StationActions  = require '../actions/StationActions.coffee'
StationStore = require '../stores/StationStore.coffee'
Member = require './MemberComponent.coffee'

module.exports = recl
  set: ->
    if window.localStorage and @$writing then window.localStorage.setItem 'writing', @$writing.text()

  get: ->
    if window.localStorage then window.localStorage.getItem 'writing'

  stateFromStore: -> {
    audi:StationStore.getAudience()
    members:StationStore.getMembers()
    typing:StationStore.getTyping()
    station:StationStore.getStation()
  }

  getInitialState: -> @stateFromStore()

  typing: (state) ->
    if @state.typing[@state.station] isnt state
      StationActions.setTyping @state.station,state

  _blur: -> 
    MessageActions.setTyping false
    @typing false

  _focus: -> 
    MessageActions.setTyping true
    @typing true

  sendMessage: ->
    MessageActions.sendMessage @state.station,@$writing.text(),@state.audi
    @$length.text "0/69"
    @$writing.text('')
    @set()
    @typing false

  _keyDown: (e) ->
    if e.keyCode is 13
      e.preventDefault()
      @sendMessage()
      return false
    @_input()
    @set()

  _input: (e) ->
    text   = @$writing.text()
    length = text.length
    geturl = new RegExp "(^|[ \t\r\n])((ftp|http|https|gopher|mailto|news|nntp|telnet|wais|file|prospero|aim|webcal):(([A-Za-z0-9$_.+!*(),;/?:@&~=-])|%[A-Fa-f0-9]{2}){2,}(#([a-zA-Z0-9][a-zA-Z0-9$_.+!*(),;/?:@&~=%-]*))?([A-Za-z0-9$_+!*();/?:~-]))", "g"
    urls = text.match(geturl)
    if urls isnt null and urls.length > 0
      for url in urls
        length -= url.length
        length += 10
    @$length.text "#{length}/69"
    if length >= 69
      @$writing.text(@$writing.text().substr(0,69))
      @cursorAtEnd()
      e.preventDefault() if e
      return false

  _setFocus: -> @$writing.focus()

  getTime: ->
    d = new Date()
    seconds = d.getSeconds()
    if seconds < 10
      seconds = "0" + seconds
    "~"+d.getHours() + "." + d.getMinutes() + "." + seconds

  cursorAtEnd: ->
    range = document.createRange()
    range.selectNodeContents @$writing[0]
    range.collapse(false)
    selection = window.getSelection()
    selection.removeAllRanges()
    selection.addRange(range)

  componentDidMount: ->
    window.util.sendMessage = @sendMessage
    StationStore.addChangeListener @_onChangeStore
    @$el = $ @getDOMNode()
    @$length = $('#length')
    @$writing = $('#writing')
    @$writing.focus()
    if @get() 
      @$writing.text @get()
      @_input()
    @interval = setInterval =>
        @$el.find('.time').text @getTime()
      , 1000

  componentWillUnmount: ->
    StationStore.removeChangeListener @_onChangeStore
    clearInterval @interval

  _onChangeStore: -> @setState @stateFromStore()

  render: ->
    user = "~"+window.urb.user
    iden = StationStore.getMember(user)
    ship = if iden then iden.ship else user
    name = if iden then iden.name else ""

    k = "writing"
    k+= " hidden" if not @state?.station

    div {className:k,onClick:@_setFocus}, [
      (div {className:"attr"}, [
        (Member iden, "")
        (div {className:"time"}, @getTime())        
      ])
      (div {
          id:"writing"
          contentEditable:true
          onFocus: @_focus
          onBlur: @_blur
          onInput: @_input
          onPaste: @_input
          onKeyDown: @_keyDown
          onFocus: @cursorAtEnd
        }, "")
      div {id:"length"}, "0/69"
      ]