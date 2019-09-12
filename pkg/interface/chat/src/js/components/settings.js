import React, { Component } from 'react';
import classnames from 'classnames';

import { ChatTabBar } from '/components/lib/chat-tabbar';


export class SettingsScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: "/" + props.match.params.station,
      isLoading: false
    };

    this.renderDelete = this.renderDelete.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    if (!!state.isLoading && !(state.station in props.inbox)) {
      this.setState({
        isLoading: false
      }, () => {
        props.history.push('/~chat');
      });
    }
  }

  deleteChat() {
    const { props, state } = this;

    props.api.inbox.delete(state.station);
    props.api.inboxSync.removeSynced(`~${props.owner}`, state.station);
    props.api.groups.unbundle(`/inbox${state.station}/read`);
    props.api.groups.unbundle(`/inbox${state.station}/write`);

    props.setSpinner(true);
    this.setState({
      isLoading: true
    });
  }

  renderDelete() {
    const { props, state } = this;

    let titleText = "Delete Chat";
    let descriptionText = "Permanently delete this chat.";
    let buttonText = "-> Delete";

    if (props.owner !== window.ship) {
      titleText = "Leave Chat"
      descriptionText = "You will no longer have access to this chat."
      buttonText = "-> Leave";
    }

    return (
      <div className="w-50 fl pl2 mt3">
        <p className="body-regular">{titleText}</p>
        <p className="label-regular gray mb3">{descriptionText}</p>
        <a onClick={this.deleteChat.bind(this)}
          className="pointer btn-font underline nice-red">{buttonText}</a>
      </div>
    );
  }

  render() {
    const { props, state } = this;

    let writeGroup = Array.from(props.group.values());

    if (!!state.isLoading) {
      let text = "Deleting...";
      if (props.owner === window.ship) {
        text = "Leaving...";
      }

      return (
        <div className="h-100 w-100 overflow-x-hidden flex flex-column">
          <div className='pl3 pt2 bb mb3'>
            <h2>{state.station.substr(1)}</h2>
            <ChatTabBar
              {...props}
              station={state.station}
              numPeers={writeGroup.length} />
          </div>
          <div className="w-100 cf pa3">
            <h2>{text}</h2>
          </div>
        </div>
      );
    }

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div className='pl3 pt2 bb mb3'>
          <h2>{state.station.substr(1)}</h2>
          <ChatTabBar
            {...props}
            station={state.station}
            numPeers={writeGroup.length}
            isOwner={props.owner === window.ship} />
        </div>
        <div className="w-100 cf pa3">
          <h2>Settings</h2>
          {this.renderDelete()}
        </div>
      </div>
    )
  }
}
