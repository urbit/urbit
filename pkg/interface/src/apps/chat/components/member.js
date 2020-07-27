import React, { Component } from 'react';

import { Link } from 'react-router-dom';

import { deSig } from '../../../lib/util';
import { ChatTabBar } from './lib/chat-tabbar';
import { MemberElement } from './lib/member-element';
import { InviteElement } from './lib/invite-element';
import { SidebarSwitcher } from '../../../components/SidebarSwitch';
import { GroupView } from '../../../components/Group';
import { PatpNoSig } from '../../../types/noun';

export class MemberScreen extends Component {
  constructor(props) {
    super(props);
    this.inviteShips = this.inviteShips.bind(this);
  }

  inviteShips(ships) {
    const { props } = this;
    return props.api.chat.invite(props.station, ships.map(s => `~${s}`));
  }

  render() {
    const { props } = this;

    const isinPopout = this.props.popout ? 'popout/' : '';

    let title = props.station.substr(1);

    if (props.association && 'metadata' in props.association) {
      title =
        props.association.metadata.title !== ''
          ? props.association.metadata.title
          : props.station.substr(1);
    }

    return (
      <div className='h-100 w-100 overflow-x-hidden flex flex-column white-d'>
        <div
          className='w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8'
          style={{ height: '1rem' }}
        >
          <Link to='/~chat/'>{'⟵ All Chats'}</Link>
        </div>
        <div
          className={`pl4 pt2 bb b--gray4 b--gray1-d bg-gray0-d flex relative
          overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0`}
          style={{ height: 48 }}
        >
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
            api={this.props.api}
          />
          <Link
            to={'/~chat/' + isinPopout + 'room' + props.station}
            className='pt2 white-d'
          >
            <h2
              className={
                'dib f9 fw4 lh-solid v-top ' +
                (title === props.station.substr(1) ? 'mono' : '')
              }
              style={{ width: 'max-content' }}
            >
              {title}
            </h2>
          </Link>
          <ChatTabBar
            {...props}
            station={props.station}
            numPeers={5}
            isOwner={deSig(props.match.params.ship) === window.ship}
            popout={this.props.popout}
            api={props.api}
          />
        </div>
        <div className='w-100 pl3 mt0 mt4-m mt4-l mt4-xl cf pr6'>
          { props.association['group-path'] && (
          <GroupView
            permissions
            group={props.group}
            resourcePath={props.association['group-path'] || ''}
            associations={props.associations}
            groups={props.groups}
            inviteShips={this.inviteShips}
            contacts={props.contacts}
          /> )}
        </div>
      </div>
    );
  }
}
