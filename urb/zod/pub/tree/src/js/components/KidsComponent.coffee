TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

recl = React.createClass
[div,a,ul,li,hr] = [React.DOM.div,React.DOM.a,React.DOM.ul,React.DOM.li,React.DOM.hr]

module.exports = recl
  displayName: "Kids"
  stateFromStore: -> 
    path = @props.dataPath ? TreeStore.getCurr()
    tree = TreeStore.getTree(path.split("/"))
    {
      path, tree,
      cont:TreeStore.getCont(),
      keys: _.keys(tree)
    }

  getInitialState: -> @stateFromStore()

  _onChangeStore: ->  @setState @stateFromStore()

  gotPath: ->
    return false unless @state.keys.length > 0
    for k in @state.keys
      return false unless @state.cont[@state.path+"/"+k]?
    true

  componentDidMount: ->
    TreeStore.addChangeListener @_onChangeStore
    TreeActions.getPath @state.path,"kids" unless @gotPath()

  render: ->
    _list = for v in _.keys(@state.tree).sort()
      _path = @state.path+"/"+v
      [(div {key:"kid-"+v}, @state.cont[_path]),(hr {},"")]
    (div {key:"kids-"+@state.path,className:"kids"}, _list)
