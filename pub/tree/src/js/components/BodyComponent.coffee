TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

load        = require './LoadComponent.coffee'

recl = React.createClass
[div,input,textarea] = [React.DOM.div,React.DOM.input,React.DOM.textarea]

module.exports = recl
  stateFromStore: -> 
    body:TreeStore.getBody()
    load:TreeStore.getLoad()
    curr:TreeStore.getCurr()
    cont:TreeStore.getCont()

  componentDidMount: -> 
    TreeStore.addChangeListener @_onChangeStore

  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore

  componentDidUpdate: (_props,_state) -> 
    if _state.curr isnt @state.curr
      setTimeout (=> @getPath _state.curr), 0

  getInitialState: -> @stateFromStore()

  _onChangeStore: ->  
    @setState @stateFromStore()

  getPath: (path) -> 
    if not @state.cont[path]? 
      TreeActions.setLoading true
      TreeActions.getPath path,=>
        TreeActions.setLoading false

  render: ->
    parts = []

    parts.push (div {id:'body',key:"body"+@state.curr},
      (@state.body ? (div {className:"loading"}, (load {}, ""))))

    (div {}, parts)