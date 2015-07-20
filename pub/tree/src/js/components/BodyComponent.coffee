TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

load        = require './LoadComponent.coffee'

recl = React.createClass
[div,input,textarea] = [React.DOM.div,React.DOM.input,React.DOM.textarea]

module.exports = recl
  displayName: "Body"
  stateFromStore: -> 
    body:TreeStore.getBody()
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
    TreeActions.getPath path unless @state.cont[path]?

  render: -> (div {},
    (div {id:'body',key:"body"+@state.curr},
      (@state.body ? (div {className:"loading"}, (load {}, "")))))
