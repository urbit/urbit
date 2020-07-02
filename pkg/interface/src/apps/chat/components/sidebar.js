import React, { Component } from 'react';
import _ from 'lodash';

import Welcome from './lib/welcome';
import { alphabetiseAssociations } from '../../../lib/util';
import { SidebarInvite } from './lib/sidebar-invite';
import { GroupItem } from './lib/group-item';
import { ShipSearchInput } from './lib/ship-search';

export class Sidebar extends Component {
  constructor() {
    super();
    this.state = {
      dmOverlay: false
    };
  }

  onClickNew() {
    this.props.history.push('/~chat/new');
  }

  onClickDm() {
    this.setState(({ dmOverlay }) => ({ dmOverlay: !dmOverlay }) );
  }

  onClickJoin() {
    this.props.history.push('/~chat/join');
  }

  goDm(ship) {
    this.setState({ dmOverlay: false }, () => {
      this.props.history.push(`/~chat/new/dm/~${ship}`);
    });
  }

  render() {
    const { props, state } = this;

    const selectedGroups = props.selectedGroups ? props.selectedGroups : [];

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
        if (groupedChannels['/~/']) {
          const array = groupedChannels['/~/'];
          array.push(box);
          groupedChannels['/~/'] = array;
        } else {
          groupedChannels['/~/'] = [box];
        }
      }
    });

    const sidebarInvites = Object.keys(props.invites)
      .map((uid) => {
        return (
          <SidebarInvite
            uid={uid}
            key={uid}
            invite={props.invites[uid]}
            api={props.api}
          />
        );
      });

    const groupedItems = Object.keys(contactAssoc)
      .filter(each => (groupedChannels[each] || []).length !== 0)
      .filter((each) => {
        if (selectedGroups.length === 0) {
          return true;
        }
        const selectedPaths = selectedGroups.map((e) => {
          return e[0];
        });
        return selectedPaths.includes(each);
      })
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
      if (groupedChannels['/~/'] && groupedChannels['/~/'].length !== 0) {
        groupedItems.push(
          <GroupItem
            association={'/~/'}
            chatMetadata={chatAssoc}
            channels={groupedChannels['/~/']}
            inbox={props.inbox}
            station={props.station}
            unreads={props.unreads}
            index={'/~/'}
            key={'/~/'}
            {...props}
          />
        );
      }
    const candidates = state.dmOverlay
          ? _.chain(this.props.contacts)
               .values()
               .map(_.keys)
               .flatten()
               .uniq()
               .value()
          : [];

    return (
      <div
        className={`h-100-minus-96-s h-100 w-100 overflow-x-hidden flex
      bg-gray0-d flex-column relative z1`}
      >
        <div className="w-100 bg-transparent pa4">
          <a
            className="dib f9 pointer green2 gray4-d mr4"
            onClick={this.onClickNew.bind(this)}
          >
            New Chat
          </a>

          <div className="dib relative mr4">
            { state.dmOverlay && (
              <ShipSearchInput
                className="absolute"
                contacts={{}}
                candidates={candidates}
                onSelect={this.goDm.bind(this)}
                onClear={this.onClickDm.bind(this)}

              />
            )}
          <a
            className="f9 pointer green2 gray4-d"
            onClick={this.onClickDm.bind(this)}
          >
            DM
          </a>
          </div>

          <a
            className="dib f9 pointer gray4-d"
            onClick={this.onClickJoin.bind(this)}
          >
            Join Chat
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
