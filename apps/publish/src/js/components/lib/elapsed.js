import React, { Component } from 'react';
import { secToString, esoo } from '/lib/util';
// display elapsed time by converting galactic time to client time

/*
  Goes from:
    1531938314116       // "javascript unix time"
  To:
    4m                  // "elapsed timestring from current time"
*/

export class Elapsed extends Component {
  constructor(props) {
    super(props);
    // console.log('elapsed props...', props);
  }

  renderTime() {
    let parsed = esoo(this.props.timestring);
    const serverTime = new Date(parsed ? parsed : this.props.timestring);
    const clientTime = new Date(); // local
    return secToString((clientTime - serverTime) / 1000).split(' ')[0];
  }

  render() {
    return (
      <span className={this.props.classes}>-{this.renderTime()}</span>
    )
  }
}
