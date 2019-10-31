import React, { Component } from 'react';
import classnames from 'classnames';
import _ from 'lodash';

export class SidebarInvite extends Component {

  onAccept() {
    this.props.api.invite.accept(this.props.uid);
  }

  onDecline() {
    this.props.api.invite.decline(this.props.uid);
  }

  render() {
    const { props } = this;

    return (
      <div className='pa3'>
        <div className='w-100 v-mid'>
          <div className="dib mr2 bg-nice-green" style={{
            borderRadius: 12,
            width: 12,
            height: 12
          }}></div>
          <p className="dib body-regular fw-normal">
            {props.invite.text}
          </p>
        </div>
        <a className="dib w-50 pointer btn-font nice-green underline" onClick={this.onAccept.bind(this)}>Accept</a>
        <a className="dib w-50 tr pointer btn-font nice-red underline" onClick={this.onDecline.bind(this)}>Decline</a>
      </div>
    )
  }
}

