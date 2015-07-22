TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

recl = React.createClass
[div,a,ul,li,hr] = [React.DOM.div,React.DOM.a,React.DOM.ul,React.DOM.li,React.DOM.hr]

module.exports = recl
  displayName: "Kids"
  stateFromStore: -> 
    path = @props.dataPath ? TreeStore.getCurr()
    { 
      path
      cont:TreeStore.getCont()
      tree:TreeStore.getTree(path.split("/"))
    }

  getInitialState: -> @stateFromStore()

  _onChangeStore: ->  @setState @stateFromStore()

  gotPath: ->
    _keys = _(@state.tree).keys()
    (not _keys.isEmpty()) and _keys.every (k) =>
      @state.cont[@state.path+"/"+k]?

  componentDidMount: ->
    TreeStore.addChangeListener @_onChangeStore
    TreeActions.getPath @state.path,"kids" unless @gotPath()

  render: ->
    div {key:"kids-"+@state.path,className:"kids"},
      for v in _.keys(@state.tree).sort()
        [(div {key:v}, @state.cont[@state.path+"/"+v]),(hr {},"")]
