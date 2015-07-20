TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

load        = React.createFactory require './LoadComponent.coffee'

recl = React.createClass
[div,a,ul,li,h1] = [React.DOM.div,React.DOM.a,React.DOM.ul,React.DOM.li,React.DOM.h1]

module.exports = recl
  displayName: "List"
  stateFromStore: -> 
    path = @props.dataPath ? TreeStore.getCurr()
    tree = TreeStore.getTree(path.split("/"))
    {
      path, tree,
      snip:TreeStore.getSnip(),
      keys: _.keys(tree)
    }

  _onChangeStore: ->
    @setState @stateFromStore()

  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore

  getInitialState: -> @stateFromStore()

  gotPath: ->
    return false unless @state.keys.length > 0
    for k in @state.keys
      unless @state.snip[@state.path+"/"+k]?
        return false
    true

  componentDidMount: ->
    TreeStore.addChangeListener @_onChangeStore
    TreeActions.getPath @state.path, "snip" unless @gotPath()

  renderList: ->
    if not @gotPath()
      return (div {className:"loading",key:""}, (load {}, ""))
    _keys = _.keys(@state.tree).sort()
    if @props.dataType is 'post' then _keys=_keys.reverse()
    for v in _keys
      _k = ""
      _path = @state.path+"/"+v
      if @props.dataPreview?
        c = "preview"
        prev = @state.snip[_path]
        if @props.titlesOnly
          prev = prev.head
        else
          prev = [prev.head, prev.body]

        if @props.dataType is 'post'
          orig = @state.snip[_path].orig
          c = orig.body.c.slice(0,2)
          c.unshift orig.head
          prev = 
            gn: 'div'
            c: c
          _k += " post"
          prev = window.tree.reactify prev
      else
        c = ""
        prev = (h1 {},v)
      href = window.tree.basepath _path
      (li {className:_k,key:"list-a-"+_path}, (a {href:href,className:c}, prev))
        
  render: ->
    k = "list"
    if @props['data-source'] is 'default' then k += " default"
    if @props.dataType is 'post' then k += " posts"
    (ul {className:k,key:"list-"+@state.path}, @renderList())
