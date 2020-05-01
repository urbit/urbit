import React, { Component } from 'react';

export class History extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div
        className="relative flex flex-column-reverse overflow-container flex-auto"
        style={{ height: 'calc(100% - 1rem)', resize: 'none' }}
      >
        <div style={{ marginTop: 'auto' }}>
          {this.props.commandLog.map((text, index) => {
            return (
              <p className="mono" key={index}
              style={{ overflowWrap: 'break-word' }}
              >
                {text}
              </p>
            );
          })}
        </div>
      </div>
    );
    }
  }

export default History;
