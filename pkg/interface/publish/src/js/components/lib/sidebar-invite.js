import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';

export class SidebarInvite extends Component {

  onAccept() {
    let action = {
      accept: {
        path: '/publish',
        uid: this.props.uid,
      }
    }
    window.api.action("invite-store", "invite-action", action);
  }

  onDecline() {
    let action = {
      decline: {
        path: '/publish',
        uid: this.props.uid,
      }
    }
    window.api.action("invite-store", "invite-action", action);
  }

  render() {
    const { props } = this;

    return (
      <div className='pa3 bb b--gray4 b--gray1-d'>
        <div className='w-100 v-mid'>
          <p className="dib f9 mono gray4-d">
            {props.invite.text}
          </p>
        </div>
        <a
          className="dib pointer pa2 f9 bg-green2 white mt4"
          onClick={this.onAccept.bind(this)}>
          Accept Invite
        </a>
        <a
          className="dib pointer ml4 pa2 f9 bg-black bg-gray0-d white mt4"
          onClick={this.onDecline.bind(this)}>
          Decline
        </a>
      </div>
    )
  }
}

