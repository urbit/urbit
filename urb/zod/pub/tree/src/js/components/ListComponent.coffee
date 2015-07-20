TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

load        = require './LoadComponent.coffee'

recl = React.createClass
[div,a,ul,li,h1] = [React.DOM.div,React.DOM.a,React.DOM.ul,React.DOM.li,React.DOM.h1]

module.exports = recl
  stateFromStore: -> 
    path = @props.dataPath ? TreeStore.getCurr()
    {
      snip:TreeStore.getSnip()
      tree:TreeStore.getTree(path.split("/"))
      path:path
    }

  _onChangeStore: ->
    @setState @stateFromStore()

  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore

  getInitialState: -> @stateFromStore()

  getCont: ->
    cont = true
    keys = _.keys @state.tree
    for k in keys
      cont = false if not @state.snip[@state.path+"/"+k]
    cont = false if keys.length is 0
    cont

  componentDidMount: ->
    cont = @getCont()
    TreeStore.addChangeListener @_onChangeStore
    if not @state.tree or _.keys(@state.tree).length is 0 or not cont
      TreeActions.getPath @state.path,"snip"

  render: ->
    doc = @state.tree ? []

    if not @getCont()
      _list = (div {className:"loading"}, (load {}, ""))
    else
      _keys = _.keys(doc).sort()
      if @props.dataType is 'post' then _keys=_keys.reverse()
      _list = _.map _keys,(v) =>
        _k = ""
        _path = @state.path+"/"+v
        if @props.dataPreview?
          c = "preview"
          if @props.titlesOnly
            prev = @state.snip[_path].head
          else
            prev = @state.snip[_path]

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
        (li {className:_k}, (a {href:href,className:c,key:"list-a-"+_path}, prev))
    k = "list"
    if @props['data-source'] is 'default' then k += " default"
    if @props.dataType is 'post' then k += " posts"
    (ul {className:k,key:"list-"+@state.path}, _list)