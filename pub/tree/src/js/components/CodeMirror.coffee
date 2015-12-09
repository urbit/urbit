recl = React.createClass
{div,textarea} = React.DOM

module.exports = recl
  render: -> div {}, textarea ref:'ed', value:@props.value
  componentDidMount: ->
    CodeMirror.fromTextArea @refs.ed.getDOMNode(),
      _.extend {readOnly:true,lineNumbers:true}, @props
