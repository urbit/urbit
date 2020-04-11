import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';

export class SidebarInvite extends Component {

  onAccept() {
    const { props } = this;
    props.api.invite.accept(props.uid);
    props.history.push(`/~groups${props.invite.path}`);
  }

  onDecline() {
    this.props.api.invite.decline(this.props.uid);
  }

  render() {
    const { props } = this;

    return (
      <div className='pa3'>
        <div className='w-100 v-mid'>
          <p className="dib f8 mono white-d">
            You have been invited to join {props.invite.path}
          </p>
        </div>
        <a className="dib pointer pa2 f9 bg-green2 white mt4" onClick={this.onAccept.bind(this)}>Accept Invite</a>
        <a className="dib pointer ml4 pa2 f9 bg-black white mt4" onClick={this.onDecline.bind(this)}>Decline</a>
      </div>
    )
  }
}

