import React, { Component } from 'react';
import _ from 'lodash';

import Welcome from '/components/lib/welcome.js';
import { alphabetiseAssociations } from '../lib/util';
import { SidebarInvite } from '/components/lib/sidebar-invite';
import { GroupItem } from '/components/lib/group-item';
import { ShipSearchInput } from '/components/lib/ship-search';


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
    this.setState(({ dmOverlay }) => ({ dmOverlay: !dmOverlay }) )
  }

  onClickJoin() {
    this.props.history.push('/~chat/join')
  }

  goDm(ship) {
    this.setState({ dmOverlay: false }, () => {
      this.props.history.push(`/~chat/new/dm/~${ship}`)
    });
  }

  render() {
    const { props, state } = this;

    let selectedGroups = !!props.selectedGroups ? props.selectedGroups : [];

    let associations = alphabetiseAssociations(props.associations.contacts);

    let groupedChannels = {};
    Object.keys(props.inbox).map((box) => {
      if (box.startsWith("/~/")) {
        if (groupedChannels["/~/"]) {
          let array = groupedChannels["/~/"];
          array.push(box);
          groupedChannels["/~/"] = array;
        } else {
          groupedChannels["/~/"] = [box]
        }
      }
      let path = !!props.associations.chat[box]
        ? props.associations.chat[box]["group-path"] : box;
      if (path in associations) {
        if (groupedChannels[path]) {
          let array = groupedChannels[path];
          array.push(box);
          groupedChannels[path] = array;
        } else {
          groupedChannels[path] = [box];
        }
      }
    });

    let sidebarInvites = Object.keys(props.invites)
      .map((uid) => {
        return (
          <SidebarInvite
            uid={uid}
            invite={props.invites[uid]}
            api={props.api} />
        );
      });

    let groupedItems = Object.keys(associations)
      .filter((each) => (groupedChannels[each] || []).length !== 0)
      .filter((each) => {
        if (selectedGroups.length === 0) {
          return true;
        }
        let selectedPaths = selectedGroups.map((e) => {return e[0]});
        return selectedPaths.includes(each)
      })
      .map((each, i) => {
        let channels = groupedChannels[each] || [];
        return(
          <GroupItem
            key={i}
            index={i}
            association={associations[each]}
            chatMetadata={props.associations["chat"]}
            channels={channels}
            inbox={props.inbox}
            station={props.station}
            unreads={props.unreads}
            {...props}
          />
        )
      });
      if (groupedChannels["/~/"] && groupedChannels["/~/"].length !== 0) {
        groupedItems.push(
          <GroupItem
            association={"/~/"}
            chatMetadata={props.associations["chat"]}
            channels={groupedChannels["/~/"]}
            inbox={props.inbox}
            station={props.station}
            unreads={props.unreads}
            index={"/~/"}
            key={"/~/"}
            {...props}
          />
        )
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
      bg-gray0-d flex-column relative z1`}>
        <div className="w-100 bg-transparent pa4">
          <a
            className="dib f9 pointer green2 gray4-d mr4"
            onClick={this.onClickNew.bind(this)}>
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
            onClick={this.onClickDm.bind(this)}>
            DM
          </a>
          </div>

          <a
            className="dib f9 pointer gray4-d"
            onClick={this.onClickJoin.bind(this)}>
            Join Chat
          </a>
        </div>
        <div className="overflow-y-auto h-100">
          <Welcome inbox={props.inbox}/>
          {sidebarInvites}
          {groupedItems}
        </div>
      </div>
    );
  }
}
