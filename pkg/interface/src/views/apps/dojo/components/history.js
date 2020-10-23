import React, { Component } from 'react';

import Line from './line';

export class History extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div
        className="h-100 relative flex flex-column-reverse overflow-container flex-auto"
        style={{ resize: 'none' }}
      >
        <div style={{ marginTop: 'auto' }}>
          {this.props.log.map((line, i) => {
            return <Line key={i} index={i} line={line} />;
          })}
        </div>
      </div>
    );
    }
  }

export default History;
