reactify  = React.createFactory require './Reactify.coffee'

TreeStore   = require '../stores/TreeStore.coffee'
TreeActions = require '../actions/TreeActions.coffee'

recl = React.createClass
div  = React.DOM.div

module.exports = recl
  displayName: "Body"
  stateFromStore: -> 
    body:TreeStore.getBody()
    curr:TreeStore.getCurr()

  componentDidMount: -> 
    TreeStore.addChangeListener @_onChangeStore

  componentWillUnmount: ->
    TreeStore.removeChangeListener @_onChangeStore

  componentDidUpdate: (_props,_state) -> 
    if _state.curr isnt @state.curr
      console.log "this wasn't happening"
      #setTimeout (=> @getPath _state.curr), 0

  getInitialState: -> @stateFromStore()

  _onChangeStore: ->  
    @setState @stateFromStore()
 
  render: -> (div {},
    (div {id:'body',key:"body"+@state.curr}, (reactify manx: @state.body)))
