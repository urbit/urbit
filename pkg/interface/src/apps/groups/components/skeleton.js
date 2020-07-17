import React, { Component } from 'react';
import { GroupSidebar } from './lib/group-sidebar';

export class Skeleton extends Component {
  render() {
    const { props } = this;
    const rightPanelClasses =
      props.activeDrawer === 'groups' ? 'dn flex-m flex-l flex-xl' : 'flex';

    return (
      <div className="h-100 w-100 ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl">
        <div className="cf w-100 h-100 flex ba-m ba-l ba-xl b--gray4 b--gray1-d br1">
          <GroupSidebar
            contacts={props.contacts}
            groups={props.groups}
            invites={props.invites}
            activeDrawer={props.activeDrawer}
            selected={props.selected}
            selectedGroups={props.selectedGroups}
            history={props.history}
            api={props.api}
            associations={props.associations}
          />
          <div
            className={'h-100 w-100 relative ' + rightPanelClasses}
            style={{ flexGrow: 1 }}
          >
            {props.children}
          </div>
        </div>
      </div>
    );
  }
}
