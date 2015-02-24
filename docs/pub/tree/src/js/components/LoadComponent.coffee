recl = React.createClass
[div,input,textarea] = [React.DOM.div,React.DOM.input,React.DOM.textarea]

module.exports = recl
  getInitialState: -> {anim: 0}
  
  componentDidMount: -> @interval = setInterval @setAnim, 100

  componentWillUnmount: -> clearInterval @interval

  setAnim: ->
    anim = @state.anim+1
    if anim > 3 then anim = 0
    @setState {anim:anim}

  render: ->
    (div {className:"spin state-#{@state.anim}"}, "")