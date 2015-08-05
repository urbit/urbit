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
    path = @props.dataPath ? TreeStore.getCurr()
    state = { 
      path
      snip:TreeStore.getSnip()
      tree:TreeStore.getTree(path.split("/"))
      tocs:@compute()
    }
    state

  _onChangeStore: ->
    @setState @stateFromStore()

  _click: (e) ->
    document.location.hash = $(e).

  componentDidMount: -> 
    @int = setInterval @checkHash,100
    @setState @stateFromStore()

  checkHash: ->
    if document.location.hash? and document.location.hash isnt @hash
      hash = document.location.hash.slice(1)
      for k,v of @state.tocs
        if v.t is hash
          @hash = document.location.hash
          $(window).scrollTop v.e.offset().top
          break

  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore
    clearInterval @int

  getInitialState: -> @stateFromStore()

  gotPath: -> TreeStore.gotSnip(@state.path)

  compute: ->
    $headers = $('#toc h1, #toc h2, #toc h3, #toc h4')
    c = []
    if $headers.length is 0 then return c
    for h in $headers
      $h = $(h)
      c.push {h:h.tagName.toLowerCase(),t:$h.text(),e:$h}
    c

  render: -> 
    (div {className:'toc'}, @state.tocs.map (i) ->
      l.push (React.DOM[i.h] {onClick:@_click}, i.t))