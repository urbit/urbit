recl = React.createClass
{div,br,input,textarea} = React.DOM

husl = require 'husl'

MessageActions  = require '../actions/MessageActions.coffee'
MessageStore    = require '../stores/MessageStore.coffee'
StationActions  = require '../actions/StationActions.coffee'
StationStore    = require '../stores/StationStore.coffee'
Member          = require './MemberComponent.coffee'

SHIPSHAPE = ///
^~?(                              #preamble
   [a-z]{3}                       # galaxy
 | [a-z]{6}(-[a-z]{6}){0,3}       # star - moon
 |   [a-z]{6}(-[a-z]{6}){3}       # comet
   (--[a-z]{6}(-[a-z]{6}){3})+    #
)$                                #postamble
///
PO = '''
dozmarbinwansamlitsighidfidlissogdirwacsabwissib
rigsoldopmodfoglidhopdardorlorhodfolrintogsilmir
holpaslacrovlivdalsatlibtabhanticpidtorbolfosdot
losdilforpilramtirwintadbicdifrocwidbisdasmidlop
rilnardapmolsanlocnovsitnidtipsicropwitnatpanmin
ritpodmottamtolsavposnapnopsomfinfonbanporworsip
ronnorbotwicsocwatdolmagpicdavbidbaltimtasmallig
sivtagpadsaldivdactansidfabtarmonranniswolmispal
lasdismaprabtobrollatlonnodnavfignomnibpagsopral
bilhaddocridmocpacravripfaltodtiltinhapmicfanpat
taclabmogsimsonpinlomrictapfirhasbosbatpochactid
havsaplindibhosdabbitbarracparloddosbortochilmac
tomdigfilfasmithobharmighinradmashalraglagfadtop
mophabnilnosmilfopfamdatnoldinhatnacrisfotribhoc
nimlarfitwalrapsarnalmoslandondanladdovrivbacpol
laptalpitnambonrostonfodponsovnocsorlavmatmipfap

zodnecbudwessevpersutletfulpensytdurwepserwylsun
rypsyxdyrnuphebpeglupdepdysputlughecryttyvsydnex
lunmeplutseppesdelsulpedtemledtulmetwenbynhexfeb
pyldulhetmevruttylwydtepbesdexsefwycburderneppur
rysrebdennutsubpetrulsynregtydsupsemwynrecmegnet
secmulnymtevwebsummutnyxrextebfushepbenmuswyxsym
selrucdecwexsyrwetdylmynmesdetbetbeltuxtugmyrpel
syptermebsetdutdegtexsurfeltudnuxruxrenwytnubmed
lytdusnebrumtynseglyxpunresredfunrevrefmectedrus
bexlebduxrynnumpyxrygryxfeptyrtustyclegnemfermer
tenlusnussyltecmexpubrymtucfyllepdebbermughuttun
bylsudpemdevlurdefbusbeprunmelpexdytbyttyplevmyl
wedducfurfexnulluclennerlexrupnedlecrydlydfenwel
nydhusrelrudneshesfetdesretdunlernyrsebhulryllud
remlysfynwerrycsugnysnyllyndyndemluxfedsedbecmun
lyrtesmudnytbyrsenwegfyrmurtelreptegpecnelnevfes
'''

Audience = recl
  displayName: "Audience"
  onKeyDown: (e) ->
    if e.keyCode is 13 
      e.preventDefault()
      setTimeout () ->
          $('#writing').focus()
        ,0
      return false
  render: ->
    div {
          id:"audi"
          className:"audi valid-#{@props.valid}"
          contentEditable:true
          @onKeyDown
          onBlur:@props.onBlur
        }, @props.audi.join(" ")

module.exports = recl
  displayName: "Writing"
  set: ->
    if window.localStorage and @$writing then window.localStorage.setItem 'writing', @$writing.text()

  get: ->
    if window.localStorage then window.localStorage.getItem 'writing'

  stateFromStore: -> 
    s =
      audi:StationStore.getAudience()
      ludi:MessageStore.getLastAudience()
      config:StationStore.getConfigs()
      members:StationStore.getMembers()
      typing:StationStore.getTyping()
      valid:StationStore.getValidAudience()
    s.audi = _.without s.audi, window.util.mainStationPath window.urb.user
    s.ludi = _.without s.ludi, window.util.mainStationPath window.urb.user
    s

  getInitialState: -> _.extend @stateFromStore(), length:0, lengthy: false

  typing: (state) ->
    if @state.typing[@state.station] isnt state
      StationActions.setTyping @state.station,state

  onBlur: -> 
    @$writing.text @$writing.text()
    MessageActions.setTyping false
    @typing false

  onFocus: -> 
    MessageActions.setTyping true
    @typing true
    @cursorAtEnd

  addCC: (audi) ->
    if window.urb.user isnt window.urb.ship #foreign
      return audi
    listening = @state.config[window.util.mainStation(window.urb.user)]?.sources ? []
    cc = false
    for s in audi
      if listening.indexOf(s) is -1
        cc = true
    if listening.length is 0 then cc = true
    if cc is true
      audi.push window.util.mainStationPath(window.urb.user)
    audi

  sendMessage: ->
    if @_validateAudi() is false
      $('#audi').focus()
      return
    if @state.audi.length is 0 and $('#audi').text().trim().length > 0
      audi = if @_setAudi() then @_setAudi() else @state.ludi
    else
      audi = @state.audi    
    audi = @addCC audi
    txt = @$writing.text().trim().replace(/\xa0/g,' ')
    MessageActions.sendMessage txt,audi
    @$writing.text('')
    @setState length:0
    @set()
    @typing false

  onKeyUp: (e) ->
    if not window.urb.util.isURL @$writing.text()
      @setState lengthy: (@$writing.text().length > 62)
    # r = window.getSelection().getRangeAt(0).cloneRange()
    # @$writing.text @$writing.text()
    # setTimeout => 
    #     s = window.getSelection()
    #     s.removeAllRanges()
    #     s.addRange r
    #     console.log r
    #   ,0
  
  onKeyDown: (e) ->
    if e.keyCode is 13
      txt = @$writing.text()
      e.preventDefault()
      if txt.length > 0
        if window.talk.online
          @sendMessage()
        else
          #@errHue = ((@errHue || 0) + (Math.random() * 300) + 30) % 360
          #$('#offline').css color: husl.toHex @errHue, 90, 50 
          $('#offline').addClass('error').one 'transitionend',
            -> $('#offline').removeClass 'error'
      return false
    @onInput()
    @set()

  onInput: (e) ->
    text   = @$writing.text()
    length = text.length
    # geturl = new RegExp [
    #  '(^|[ \t\r\n])((ftp|http|https|gopher|mailto|'
    #     'news|nntp|telnet|wais|file|prospero|aim|webcal'
    #  '):(([A-Za-z0-9$_.+!*(),;/?:@&~=-])|%[A-Fa-f0-9]{2}){2,}'
    #  '(#([a-zA-Z0-9][a-zA-Z0-9$_.+!*(),;/?:@&~=%-]*))?'
    #  '([A-Za-z0-9$_+!*();/?:~-]))'
    #                      ].join() , "g"
    # urls = text.match(geturl)
    # if urls isnt null and urls.length > 0
    #   for url in urls
    #     length -= url.length
    #     length += 10
    @setState {length}

  _validateAudiPart: (a) ->
    a = a.trim()
    # if a[0] isnt "~"
    #   return false
    if a.indexOf("/") isnt -1
      _a = a.split("/")
      if _a[1].length is 0
        return false
      ship = _a[0]
    else
      ship = a
     
    return (SHIPSHAPE.test ship) and 
      _.all (ship.match /[a-z]{3}/g), (a)-> -1 isnt PO.indexOf a

  _validateAudi: ->
    v = $('#audi').text()
    v = v.trim()
    if v.length is 0 
      return true
    if v.length < 5 # zod/a is shortest
      return false
    _.all (v.split /\ +/), @_validateAudiPart

  _setAudi: ->
    valid = @_validateAudi()
    StationActions.setValidAudience valid
    if valid is true
      stan = $('#audi').text() || window.util.mainStationPath window.urb.user
      stan = (stan.split /\ +/).map (v)->
        if v[0] is "~" then v else "~"+v
      StationActions.setAudience stan
      stan
    else
      false

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
    MessageStore.addChangeListener @_onChangeStore
    @$el = $ @getDOMNode()
    @$writing = $('#writing')
    @$writing.focus()
    if @get() 
      @$writing.text @get()
      @onInput()
    @interval = setInterval =>
        @$el.find('.time').text @getTime()
      , 1000

  componentWillUnmount: ->
    StationStore.removeChangeListener @_onChangeStore
    clearInterval @interval

  _onChangeStore: -> @setState @stateFromStore()

  render: ->
    # if window.urb.user isnt window.urb.ship #foreign
    #   return div {className:"writing"}
    
    user = "~"+window.urb.user
    iden = StationStore.getMember(user)
    ship = if iden then iden.ship else user
    name = if iden then iden.name else ""

    audi = if @state.audi.length is 0 then @state.ludi else @state.audi
    audi = window.util.clipAudi audi
    for k,v of audi
      audi[k] = v.slice(1)

    k = "writing"
    
    div {className:k}, [
      (div {className:"attr"}, [
        (React.createElement Member, iden)
        (React.createElement Audience, {audi,valid:@state.valid, onBlur:@_setAudi})
        (div {className:"time"}, @getTime())        
      ])
      (div {
          id:"writing"
          contentEditable:true
          onPaste: @onInput
          @onInput, @onFocus, @onBlur, @onKeyDown, @onKeyUp
        }, "")
       (div {id:"length"}, "#{@state.length}/64 (#{Math.ceil @state.length / 64})")
      ]
