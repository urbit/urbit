_load = require './LoadComponent.coffee'

TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

recl = React.createClass
{div,span,code} = React.DOM

module.exports = (queries, Child, load=_load)-> recl
  displayName: "Async"
  
  getInitialState: -> @stateFromStore()
  _onChangeStore: ->  
    @setState @stateFromStore()
  
  getPath: -> @props.dataPath ? TreeStore.getCurr()
  stateFromStore: -> got: TreeStore.fulfill @getPath(), queries
  
  componentDidMount: -> 
    TreeStore.addChangeListener @_onChangeStore
    @checkPath()
    
  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore
    
  componentDidUpdate: (_props,_state) ->
    if _props isnt @props
      @setState @stateFromStore()
    @checkPath()
    
  checkPath: -> TreeActions.sendQuery @getPath(), @filterQueries()
  
  filterQueries: -> @filterWith @state.got, queries
  filterWith: (have,_queries)->
    return _queries unless have?
    request = {}
    for k of _queries
      request[k] = _queries[k] unless have[k] isnt undefined
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
  
  scrollHash: -> @getHashElement()?.scrollIntoView()
  getHashElement: ->
    {hash} = document.location
    if hash then document.getElementById hash[1..]

  render: -> div {},
    if @filterQueries()?
      React.createElement load, @props
    else
      if not @getHashElement()        # onmount?
        setTimeout @scrollHash,0
      React.createElement Child,
        (_.extend {}, @props, @state.got),
        @props.children
