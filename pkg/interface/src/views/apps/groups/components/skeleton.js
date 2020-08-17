import React, { Component } from 'react';
import { GroupSidebar } from './lib/group-sidebar';
import ErrorBoundary from '~/views/components/ErrorBoundary';

export class Skeleton extends Component {
  render() {
    const { props } = this;
    const rightPanelClasses =
      props.activeDrawer === 'groups' ? 'dn flex-m flex-l flex-xl' : 'flex';

    return (
      <div className="h-100 w-100 ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl">
        <div className="bg-white bg-gray0-d cf w-100 h-100 flex ba-m ba-l ba-xl b--gray4 b--gray1-d br1">
          <GroupSidebar
            contacts={props.contacts}
            groups={props.groups}
            invites={props.invites}
            activeDrawer={props.activeDrawer}
            selected={props.selected}
            history={props.history}
            api={props.api}
            associations={props.associations}
          />
          <div
            className={'h-100 w-100 relative ' + rightPanelClasses}
            style={{ flexGrow: 1 }}
          >
            <ErrorBoundary>
              {props.children}
            </ErrorBoundary>
          </div>
        </div>
      </div>
    );
  }
}
