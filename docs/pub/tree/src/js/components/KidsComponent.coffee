TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

recl = React.createClass
[div,a,ul,li,hr] = [React.DOM.div,React.DOM.a,React.DOM.ul,React.DOM.li,React.DOM.hr]

module.exports = recl
  stateFromStore: -> 
    path = @props.dataPath ? TreeStore.getCurr()
    {
      cont:TreeStore.getCont()
      tree:TreeStore.getTree(path.split("/"))
      path:path
    }

  componentDidMount: -> 
    TreeStore.addChangeListener @_onChangeStore

  getInitialState: -> @stateFromStore()

  _onChangeStore: ->  @setState @stateFromStore()

  componentDidMount: ->
    cont = true
    for k in _.keys @state.tree
      cont = false if not @state.cont[@state.path+"/"+k]
    if not @state.tree or _.keys(@state.tree).length is 0 or not cont
      TreeActions.getPath @state.path,"kids"

  render: ->
    doc = @state.tree ? []
    _list = _.map _.keys(doc).sort(), (v) =>
      _path = @state.path+"/"+v
      [(div {key:"kid-"+v}, @state.cont[_path]),(hr {},"")]
    (div {key:"kids-"+@state.path,className:"kids"}, _list)