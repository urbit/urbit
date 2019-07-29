import React, { Component } from 'react';
import classnames from 'classnames';

import urbitOb from 'urbit-ob';
import { deSig } from '/lib/util';
import { ChatTabBar } from '/components/lib/chat-tabbar';
import { MemberElement } from '/components/lib/member-element';

export class MemberScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: props.match.params.ship + "/" + props.match.params.station,
      circle: props.match.params.station,
      host: props.match.params.ship,
      invMembers: '',
      error: false,
      success: false
    };

  }

  inviteMembers() {
    const { props, state } = this;
    let sis = state.invMembers.split(',')
      .map((mem) => mem.trim())
      .map(deSig);

    let isValid = true;
    sis.forEach((mem) => {
      if (!urbitOb.isValidPatp(`~${mem}`)) {
        isValid = false;
      }
    });

    if (isValid) {
      props.api.permit(state.circle, sis, true);
      if (this.textarea) {
        this.textarea.value = '';
      }
      this.setState({
        error: false,
        success: true,
        invMembers: ''
      });
    } else {
      this.setState({ error: true, success: false });
    }
  }

  inviteMembersChange(e) {
    this.setState({
      invMembers: e.target.value
    });
  }

  render() {
    const { props, state } = this;

    let peers = props.peers[state.station] || [window.ship];
    let listMembers = peers.map((mem) => {
      return (
        <MemberElement 
          key={mem} 
          host={state.host}
          ship={mem}
          circle={state.circle}
          api={props.api} />
      );
    });

    let errorElem = !!this.state.error ? (
      <p className="pa2 nice-red label-regular">Invalid ship name.</p>
    ) : (
      <div></div>
    );

    let successElem = !!this.state.success ? (
      <p className="pa2 nice-green label-regular">Sent invites!</p>
    ) : (
      <div></div>
    );


    let inviteButtonClasses = "label-regular underline black btn-font pointer";
    if (!this.state.error) {
      inviteButtonClasses = inviteButtonClasses + ' black';
    }

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div className='pl3 pt2 bb mb3'>
          <h2>{state.circle}</h2>
          <ChatTabBar
            {...props}
            station={state.station}
            numPeers={peers.length} />
        </div>
        <div className="w-100 cf">
          <div className="w-50 fl pa2">
            <p className="body-regular">Permitted Members</p>
            <p className="label-regular gray mb3">
              Everyone with permission to see this chat.
            </p>
            {listMembers}
          </div>
          { `~${window.ship}` === state.host ? (
            <div className="w-50 fr pa2">
              <p className="body-regular">Invite</p>
              <p className="label-regular gray mb3">
                Invite new participants to this chat.
              </p>
              <textarea
                ref={ e => { this.textarea = e; } }
                className="w-80 db ba overflow-y-hidden gray mb2"
                style={{
                  resize: 'none',
                  height: 150
                }}
                onChange={this.inviteMembersChange.bind(this)}></textarea>
              <button
                onClick={this.inviteMembers.bind(this)}
                className={inviteButtonClasses}>
                Invite
              </button>
              {errorElem}
              {successElem}
            </div>
          ) : null }
        </div>
      </div>
    )
  }
}
