import React, { Component } from 'react';
import classnames from 'classnames';

import { ChatTabBar } from '/components/lib/chat-tabbar';


export class SettingsScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: props.match.params.ship + "/" + props.match.params.station,
      circle: props.match.params.station,
      host: props.match.params.ship,
    };
  }

  deleteChat() {
    const { props, state } = this;
    props.api.delete(state.circle);

    if (state.station in props.store.state.messages) {
      delete props.store.state.messages[state.station];
    }

    if (state.station in props.store.state.configs) {
      delete props.store.state.configs[state.station];
    }

    props.store.state.circles = props.store.state.circles
      .filter((elem) => {
        return elem !== state.circle;
      });

    props.history.push('/~chat');
    props.store.setState(props.store.state);
  }

  render() {
    const { props, state } = this;
    let peers = props.peers[state.station] || [window.ship];

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div className='pl2 pt2 bb mb3'>
          <h2>{state.circle}</h2>
          <ChatTabBar 
            {...props}
            station={state.station} 
            numPeers={peers.length} />
        </div>
        <div className="w-100 cf pa3">
          <h2>Settings</h2>
          <div className="w-50 fl pr2 mt3">
            <p className="body-regular">Rename</p>
            <p className="label-regular gray mb3">
              Change the name of this chat.
            </p>
            <p className="label-small-mono inter">Chat Name</p>
            <input type="text" className="ba gray pa2 w-80" />
          </div>
          <div className="w-50 fr pl2 mt3">
            <p className="body-regular">Delete Chat</p>
            <p className="label-regular gray mb3">
              Permanently delete this chat.
            </p>
            <a onClick={this.deleteChat.bind(this)}
              className="pointer btn-font underline nice-red">-> Delete</a>
          </div>
        </div>
      </div>
    )
  }
}

