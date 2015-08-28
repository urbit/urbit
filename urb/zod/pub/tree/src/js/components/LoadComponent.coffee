recl = React.createClass
{div,input,textarea} = React.DOM

module.exports = recl 
  displayName: "Load"
  getInitialState: -> {anim: 0}
  
  componentDidMount: -> @interval = setInterval @setAnim, 100

  componentWillUnmount: -> clearInterval @interval

  setAnim: ->
    anim = @state.anim+1
    if anim > 3 then anim = 0
    @setState {anim:anim}

  render: -> (div {className:"loading"},
                (div {className:"spin state-#{@state.anim}"}, ""))
