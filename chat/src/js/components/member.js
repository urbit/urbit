import React, { Component } from 'react';
import classnames from 'classnames';

import { ChatTabBar } from '/components/lib/chat-tabbar';
import { MemberElement } from '/components/lib/member-element';

export class MemberScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: props.match.params.ship + "/" + props.match.params.station,
      circle: props.match.params.station,
      host: props.match.params.ship,
      invMembers: ''
    };

  }

  inviteMembers() {
    const { props, state } = this;
    let sis = state.invMembers.trim().split(',');
    console.log(sis);
    props.api.permit(state.circle, sis, true);
  }

  inviteMembersChange(e) {
    this.setState({
      invMembers: e.target.value
    });
  }

  render() {
    const { props, state } = this;

    let listMembers = [
      'zod',
      'bus'
    ].map((mem) => {
      return (
        <MemberElement 
          key={mem} 
          isHost={ props.ship === state.host }
          ship={mem}
          circle={state.circle}
          api={props.api} />
      );
    });

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div className='pl2 pt2 bb mb3'>
          <h2>{state.circle}</h2>
          <ChatTabBar {...props} station={state.station} />
        </div>
        <div className="w-100 cf">
          <div className="w-50 fl pa2">
            <p className="body-regular">Members</p>
            <p className="label-regular gray mb3">
              Everyone subscribed to this chat.
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
                className="w-80 db ba overflow-y-hidden gray mb2"
                style={{ 
                  resize: 'none',
                  height: 150
                }}
                onChange={this.inviteMembersChange.bind(this)}></textarea>
              <a 
                onClick={this.inviteMembers.bind(this)}
                className="label-regular underline gray btn-font">
                Invite
              </a>
            </div>
          ) : null }
        </div>
      </div>
    )
  }
}

