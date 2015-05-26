recl = React.createClass
[div,textarea] = [React.DOM.div,React.DOM.textarea]

module.exports = recl
  render: -> div {}, textarea ref:'ed', value:@props.value
  componentDidMount: ->
    CodeMirror.fromTextArea @refs.ed.getDOMNode(),
      readOnly:true
      lineNumbers:true
