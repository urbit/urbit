import React, { Component } from 'react';

export class Summary extends Component {
  // expected props:
  // id: 'id'
  // summary: <jsx>
  // details: <jsx>
  // onOpen: function(id)
  // onClose: function(id)

  constructor(props) {
    super(props);
    this.onToggle = this.onToggle.bind(this);
  }

  onToggle(event) {
    if (event.target.open) {
      if (this.props.onOpen) this.props.onOpen(this.props.id);
    } else {
      if (this.props.onClose) this.props.onClose(this.props.id);
    }
  }

  render() {
    const { props } = this;

    return (
      <details onToggle={this.onToggle} {...props} style={{border: '1px solid black', padding: '4px', position: 'relative', ...props.style}}>
        <summary>
          {props.summary}
        </summary>
        <div style={{borderTop: '1px solid black'}}>{props.details}</div>
      </details>
    )
  }
}