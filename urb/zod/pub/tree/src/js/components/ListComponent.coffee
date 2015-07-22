clas        = require 'classnames'

TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

load        = React.createFactory require './LoadComponent.coffee'

recl = React.createClass
[div,a,ul,li,h1] = [React.DOM.div,React.DOM.a,React.DOM.ul,React.DOM.li,React.DOM.h1]

module.exports = recl
  displayName: "List"
  stateFromStore: -> 
    path = @props.dataPath ? TreeStore.getCurr()
    { 
      path
      snip:TreeStore.getSnip()
      tree:TreeStore.getTree(path.split("/"))
    }

  _onChangeStore: ->
    @setState @stateFromStore()

  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore

  getInitialState: -> @stateFromStore()

  gotPath: ->
    _keys = _(@state.tree).keys()
    (not _keys.isEmpty()) and _keys.every (k) =>
      @state.snip[@state.path+"/"+k]?

  componentDidMount: ->
    TreeStore.addChangeListener @_onChangeStore
    TreeActions.getPath @state.path, "snip" unless @gotPath()

  renderList: ->
    if not @gotPath()
      return (div {className:"loading"}, (load {}, ""))
    _keys = _.keys(@state.tree).sort()
    if @props.dataType is 'post' then _keys=_keys.reverse()
    for item in _keys
      path = @state.path+"/"+item
      snip = @state.snip[path]
      href = window.tree.basepath path
      li {className:@props.dataType ? ""},
        a {href,className:(clas preview: @props.dataPreview?)},
          if not @props.dataPreview? then (h1 {},item)
          else if @props.dataType is 'post'
            orig = snip.orig
            head = 
              if snip.meta?.title
                 gn: 'h1'
                 c: [snip.meta.title]
               else orig.head
            window.tree.reactify
              gn: 'div'
              c: [head, (orig.body.c.slice 0,2)...]
          else if @props.titlesOnly? then snip.head
          else [snip.head, snip.body]
    
  render: ->
    k = "list"
    if @props['data-source'] is 'default' then k += " default"
    if @props.dataType is 'post' then k += " posts"
    (ul {className:k,key:"list-"+@state.path}, @renderList())
