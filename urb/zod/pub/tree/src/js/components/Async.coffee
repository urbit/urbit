load      = React.createFactory require './LoadComponent.coffee'

TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

recl = React.createClass
{div,span,code} = React.DOM

module.exports = (queries, Child)-> recl
  displayName: "Async"
  stateFromStore: ->
    path= @props.dataPath ? TreeStore.getCurr()
    {path,got: TreeStore.fulfill path, queries}
  componentDidMount: -> 
    TreeStore.addChangeListener @_onChangeStore
    @checkPath()

  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore

  componentDidUpdate: (_props,_state) -> @checkPath()
  filterQueries: -> @filterWith @state.got, queries
  filterWith: (have,_queries)->
    return _queries unless have?
    request = {}
    for k of _queries
      request[k] = _queries[k] unless have[k]?
    if _queries.kids? and have.kids?
      if _.isEmpty have.kids
        request.kids = _queries.kids
      else
        request.kids = {}
        for k,kid of have.kids
          _.merge request.kids, @filterWith kid, _queries.kids
        if _.isEmpty request.kids
          delete request.kids
    request unless _.isEmpty request

  checkPath: -> TreeActions.getPath @state.path, @filterQueries()
  getInitialState: -> @stateFromStore()
  _onChangeStore: ->  
    @setState @stateFromStore()
 
  render: -> div {},
    #span {}, JSON.stringify @filterQueries()
    if @filterQueries()?
      (div {className:"loading"}, (load {}, ""))
    else React.createElement Child,
          (_.merge @props, @state.got),
          @props.children
