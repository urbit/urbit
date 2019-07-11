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
      isLoading: false
    };
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    if (!!state.isLoading && !props.circles.includes(state.station)) {
      this.setState({
        isLoading: false
      }, () => {
        props.history.push('/~chat');
      });
    }
  }

  deleteChat() {
    const { props, state } = this;
    if (state.host === `~${window.ship}`) {
      props.api.delete(state.circle);
    } else {
      let internalCircle = 'hall-internal-' + state.circle;

      props.api.chat([
        {
          source: {
            nom: 'inbox',
            sub: false,
            srs: [state.station]
          }
        },
        {
          delete: {
            nom: internalCircle,
            why: ''
          }
        }
      ]);
    }

    props.setSpinner(true);
    this.setState({
      isLoading: true
    });
  }

  render() {
    const { props, state } = this;
    let peers = props.peers[state.station] || [window.ship];

    if (!!state.isLoading) {
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
            <h2>Deleting...</h2>
          </div>
        </div>
      );
    }

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
          <div className="w-50 fl pl2 mt3">
            <p className="body-regular">Delete Chat</p>
            <p className="label-regular gray mb3">
              Permanently delete this chat.
            </p>
            <a onClick={this.deleteChat.bind(this)}
              className="pointer btn-font underline nice-red">-> Delete</a>
          </div>
          <div className="w-50 fr pr2 mt3">
          </div>

        </div>
      </div>
    )
  }
}

