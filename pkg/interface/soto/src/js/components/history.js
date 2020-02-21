import React, { Component } from 'react';

export class History extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div className="flex flex-column-reverse overflow-container"
      style={{ height: 'calc(100% - 1rem)', resize: 'vertical' }}>
        <div style={{ marginTop: 'auto'}}>
          {this.props.commandLog.map((text, index) => {
            return <p className="mono" key={index}>{text}</p>
          })}
        </div>
      </div>
      )
    }
  }

export default History;