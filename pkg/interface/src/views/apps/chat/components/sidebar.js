import React, { Component } from 'react';

import Welcome from './lib/welcome';
import { alphabetiseAssociations } from '~/logic/lib/util';
import SidebarInvite from '~/views/components/SidebarInvite';
import { GroupItem } from './lib/group-item';

export class Sidebar extends Component {
  onClickNew() {
    this.props.history.push('/~chat/new');
  }

  render() {
    const { props } = this;

    const contactAssoc =
      (props.associations && 'contacts' in props.associations)
      ? alphabetiseAssociations(props.associations.contacts) : {};

    const chatAssoc =
      (props.associations && 'chat' in props.associations)
      ? alphabetiseAssociations(props.associations.chat) : {};

    const groupedChannels = {};
    Object.keys(props.inbox).map((box) => {
      const path = chatAssoc[box]
        ? chatAssoc[box]['group-path'] : box;

      if (path in contactAssoc) {
        if (groupedChannels[path]) {
          const array = groupedChannels[path];
          array.push(box);
          groupedChannels[path] = array;
        } else {
          groupedChannels[path] = [box];
        }
      } else {
        if (groupedChannels['dm']) {
          const array = groupedChannels['dm'];
          array.push(box);
          groupedChannels['dm'] = array;
        } else {
          groupedChannels['dm'] = [box];
        }
      }
    });

    const sidebarInvites = Object.keys(props.invites)
      .map((uid) => {
        return (
          <SidebarInvite
            key={uid}
            invite={props.invites[uid]}
            onAccept={() => props.api.invite.accept('/chat', uid)}
            onDecline={() => props.api.invite.decline('/chat', uid)}
          />
        );
      });

    const groupedItems = Object.keys(contactAssoc)
      .filter(each => (groupedChannels[each] || []).length !== 0)
      .map((each, i) => {
        const channels = groupedChannels[each] || [];
        return(
          <GroupItem
            key={i}
            index={i}
            association={contactAssoc[each]}
            chatMetadata={chatAssoc}
            channels={channels}
            inbox={props.inbox}
            station={props.station}
            unreads={props.unreads}
            {...props}
          />
        );
      });
      // add direct messages after groups
      groupedItems.push(
        <GroupItem
          association={'dm'}
          chatMetadata={chatAssoc}
          channels={groupedChannels['dm']}
          inbox={props.inbox}
          station={props.station}
          unreads={props.unreads}
          index={'dm'}
          key={'dm'}
          {...props}
        />
      );

    return (
      <div
        className={`h-100-minus-96-s h-100 w-100 overflow-x-hidden flex
      bg-gray0-d flex-column relative z1 lh-solid`}
      >
        <div className="w-100 bg-transparent pa4">
          <a
            className="dib f9 pointer green2 gray4-d mr4"
            onClick={this.onClickNew.bind(this)}
          >
            New Group Chat
          </a>
        </div>
        <div className="overflow-y-auto h-100">
          <Welcome inbox={props.inbox} />
          {sidebarInvites}
          {groupedItems}
        </div>
      </div>
    );
  }
}
