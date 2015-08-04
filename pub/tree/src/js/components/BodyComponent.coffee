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
      console.log "this wasn't happening"
      #setTimeout (=> @getPath _state.curr), 0

  getInitialState: -> @stateFromStore()

  _onChangeStore: ->  
    @setState @stateFromStore()
 
  render: -> (div {},
    (div {id:'body',key:"body"+@state.curr},
      (@state.body ? (div {className:"loading"}, (load {}, "")))))
