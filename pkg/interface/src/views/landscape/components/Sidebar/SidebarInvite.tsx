import React, { Component } from 'react';
import { Invite } from '~/types/invite-update';

export class SidebarInvite extends Component<{invite: Invite, onAccept: Function, onDecline: Function}, {}> {
  render() {
    const { props } = this;

    return (
      <div className='w-100 bg-white bg-gray0-d pa4 bb b--gray4 b--gray1-d  z-5' style={{position: 'sticky', top: 0}}>
        <div className='w-100 v-mid'>
          <p className="dib f8 mono gray4-d">
            {props.invite.text ? props.invite.text : props.invite.path}
          </p>
        </div>
        <a
          className="dib pointer pa2 f9 bg-green2 white mt4"
          onClick={this.props.onAccept.bind(this)}
        >
          Accept Invite
        </a>
        <a
          className="dib pointer ml4 pa2 f9 bg-black bg-gray0-d white mt4"
          onClick={this.props.onDecline.bind(this)}
        >
          Decline
        </a>
      </div>
    );
  }
}

export default SidebarInvite;