clas        = require 'classnames'

TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

load        = React.createFactory require './LoadComponent.coffee'
reactify    = (manx)-> React.createElement window.tree.reactify, {manx}

recl = React.createClass
[div,a,ul,li,] = [React.DOM.div,React.DOM.a,React.DOM.ul,React.DOM.li,React.DOM.h1]

module.exports = recl
  hash:null
  displayName: "TableofContents"
  stateFromStore: ->
    { 
      body:TreeStore.getBody()
    }

  _onChangeStore: ->
    @setState @stateFromStore()

  _click: (e) ->
    document.location.hash = @urlsafe $(e.target).text()

  urlsafe: (str) -> str.toLowerCase().replace(/\ /g, "-")

  componentDidMount: -> 
    @int = setInterval @checkHash,100
    @st = $(window).scrollTop()
    $(window).on 'scroll',@checkScroll
    @$headers = $('#toc h1, #toc h2, #toc h3, #toc h4')

  checkScroll: ->
    st = $(window).scrollTop()
    if Math.abs(@st-st) > 10
      hash = null
      @st = st
      for k,v of @$headers
        continue if v.tagName is undefined
        $h = $ v
        hst = $h.offset().top-$h.outerHeight(true)
        if hst < st
          hash = @urlsafe $h.text()
        if hst > st and hash isnt @hash and hash isnt null
          @hash = "#"+hash
          document.location.hash = hash
          break

  checkHash: ->
    if document.location.hash?.length > 0 and document.location.hash isnt @hash
      hash = document.location.hash.slice(1)
      for k,v of @$headers
        $h = $ v
        if hash is @urlsafe $h.text()
          @hash = document.location.hash
          offset = $h.offset().top - $h.outerHeight(true)
          setTimeout -> $(window).scrollTop offset
            , 10
          break

  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore
    clearInterval @int

  getInitialState: -> @stateFromStore()

  gotPath: -> TreeStore.gotSnip(@state.path)

  collectHeaders: (e) ->
    hs = [{gn:"h1", ga:{className:"t"}, c:["Table of contents"]}]
    for k,v of e
      if not v.gn then continue
      if v.gn[0] is 'h' and parseInt(v.gn[1]) isnt NaN
        hs.push v
    return hs

  parseHeaders: ->
    if @state.body.c
      for k,v of @state.body.c
        if v.gn is 'div' and v.ga?.id is "toc"
          return {gn:"div", ga:{className:"toc",onClick:@_click}, c:@collectHeaders(v.c)}

  render: -> reactify @parseHeaders()