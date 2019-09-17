import React, { Component } from 'react';
import classnames from 'classnames';

export class PeerComponent extends Component {
  constructor(props) {
    super(props);
  }

  handleEvent(diff){
    console.log("peerComponent", diff);
  }

  render() {
    return (
      <div>
        <div>Peer Component</div>
      </div>
    );
  }
}
