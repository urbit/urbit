import React, { Component } from 'react';

import { Link } from 'react-router-dom';

import { deSig } from '../../../lib/util';
import { ChatTabBar } from './lib/chat-tabbar';
import { MemberElement } from './lib/member-element';
import { InviteElement } from './lib/invite-element';
import { SidebarSwitcher } from './lib/icons/icon-sidebar-switch.js';

export class MemberScreen extends Component {
  render() {
    const { props } = this;

    const perm = Array.from(props.permission.who.values());

    let memberText = '';
    let modifyText = '';

    if (props.permission.kind === 'black') {
      memberText = 'Everyone banned from accessing this chat.';
      modifyText = 'Ban someone from accessing this chat.';
    } else if (props.permission.kind === 'white') {
      memberText = 'Everyone with permission to access this chat.';
      modifyText = 'Invite someone to this chat.';
    }

    const contacts = (props.station in props.contacts)
      ? props.contacts[props.station] : {};

    const members = perm.map((mem) => {
      const contact = (mem in contacts)
        ? contacts[mem] : false;

      return (
        <MemberElement
          key={mem}
          owner={deSig(props.match.params.ship)}
          contact={contact}
          ship={mem}
          path={props.station}
          kind={props.permission.kind}
          api={props.api}
        />
      );
    });

    const isinPopout = this.props.popout ? 'popout/' : '';

    let title = props.station.substr(1);

    if (props.association && 'metadata' in props.association) {
      title =
        props.association.metadata.title !== ''
          ? props.association.metadata.title
          : props.station.substr(1);
    }

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: '1rem' }}
        >
          <Link to="/~chat/">{'⟵ All Chats'}</Link>
        </div>
        <div
          className={`pl4 pt2 bb b--gray4 b--gray1-d bg-gray0-d flex relative
          overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0`}
          style={{ height: 48 }}
        >
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
          />
          <Link to={'/~chat/' + isinPopout + 'room' + props.station}
          className="pt2 white-d"
          >
            <h2
              className={'dib f9 fw4 lh-solid v-top ' +
                ((title === props.station.substr(1)) ? 'mono' : '')}
              style={{ width: 'max-content' }}
            >
              {title}
            </h2>
          </Link>
          <ChatTabBar
            {...props}
            station={props.station}
            numPeers={perm.length}
            isOwner={deSig(props.match.params.ship) === window.ship}
            popout={this.props.popout}
            api={props.api}
          />
        </div>
        <div className="w-100 pl3 mt0 mt4-m mt4-l mt4-xl cf pr6">
          <div className="w-100 w-50-l w-50-xl fl pa2 pr3 pt3 pt0-l pt0-xl">
            <p className="f8 pb2">Modify Permissions</p>
            <p className="f9 gray2 mb3">{modifyText}</p>
            {window.ship === deSig(props.match.params.ship) ? (
              <InviteElement
                path={props.station}
                permissions={props.permission}
                contacts={props.contacts}
                api={props.api}
              />
            ) : null}
          </div>
          <div className="w-100 w-50-l w-50-xl fl pa2 pr3 pt3 pt0-l pt0-xl">
            <p className="f8 pb2">Members</p>
            <p className="f9 gray2 mb3">{memberText}</p>
            {members}
          </div>
        </div>
      </div>
    );
  }
}
