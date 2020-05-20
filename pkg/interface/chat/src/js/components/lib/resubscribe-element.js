import React, { Component } from 'react';
import classnames from 'classnames';


export class ResubscribeElement extends Component {

  onClickResubscribe() {
    this.props.api.chatHook.addSynced(
      this.props.host,
      this.props.station,
      true);
  }

  render() {
    let props = this.props;

    return (
      <div className="db pa3 ma3 ba b--yellow2 bg-yellow0">
        <p className="lh-copy db">
          Your ship has been disconnected from the chat's host.
          This may be due to a bad connection, going offline, lack of permission,
          or an over-the-air update.
        </p>
        <a onClick={this.onClickResubscribe.bind(this)}
           className="db underline black pointer mt3">
          Reconnect to this chat
        </a>
      </div>
    );
  }
}
