import React, { Component } from 'react';

import { Link } from 'react-router-dom';
import { GroupItem } from './group-item';
import SidebarInvite from '~/views/components/SidebarInvite';
import { Welcome } from './welcome';
import { alphabetiseAssociations } from '~/logic/lib/util';

export const ChannelSidebar = (props) => {
  const sidebarInvites = Object.keys(props.invites)
    .map((uid) => {
      return (
        <SidebarInvite
          key={uid}
          invite={props.invites[uid]}
          onAccept={() => props.api.invite.accept('/link', uid)}
          onDecline={() => props.api.invite.decline('/link', uid)}
        />
      );
    });

  const associations = props.associations.contacts ?
    alphabetiseAssociations(props.associations.contacts) : {};

  const graphAssoc = props.associations.graph || {};

  const groupedChannels = {};
  [...props.graphKeys].map((gKey) => {
    const path = `/ship/~${gKey.split('/')[0]}/${gKey.split('/')[1]}`;
    const groupPath = graphAssoc[path] ? graphAssoc[path]['group-path'] : '';

    if (groupPath in associations) {
      // managed

      if (groupedChannels[groupPath]) {
        const array = groupedChannels[groupPath];
        array.push(path);
        groupedChannels[groupPath] = array;
      } else {
        groupedChannels[groupPath] = [path];
      }

    } else {
      // unmanaged

      if (groupedChannels['/~/']) {
        const array = groupedChannels['/~/'];
        array.push(path);
        groupedChannels['/~/'] = array;
      } else {
        groupedChannels['/~/'] = [path];
      }
    }
  });

  const groupedItems = Object.keys(associations).map((each, i) => {
    const channels = groupedChannels[each];
    if (!channels || channels.length === 0) { return; }

    return (
      <GroupItem
        key={i + 1}
        unmanaged={false}
        association={associations[each]}
        metadata={graphAssoc}
        channels={channels}
        selected={props.selected}
      />
    );
  });

  if (groupedChannels['/~/'] && groupedChannels['/~/'].length !== 0) {
    groupedItems.push(
      <GroupItem
        key={0}
        unmanaged={true}
        association={'/~/'}
        metadata={graphAssoc}
        channels={groupedChannels['/~/']}
        selected={props.selected}
      />
    );
  }

  const activeClasses = (props.active === 'collections') ? ' ' : 'dn-s ';
  const hiddenClasses = !!props.popout ? false : props.sidebarShown;

  return (
    <div className={
      `bn br-m br-l br-xl b--gray4 b--gray1-d lh-copy h-100` +
      `flex-shrink-0 mw5-m mw5-l mw5-xl pt3 pt0-m pt0-l pt0-xl relative ` +
      activeClasses +
      ((hiddenClasses) ? 'flex-basis-100-s flex-basis-30-ns' : 'dn')
    }>
      <div className="overflow-y-scroll h-100">
        <div className="w-100 bg-transparent">
          <Link
            className="dib f9 pointer green2 gray4-d pa4"
            to={'/~link/new'}>
            New Collection
          </Link>
        </div>
        <Welcome associations={props.associations} />
        {sidebarInvites}
        {groupedItems}
      </div>
    </div>
  );
};

