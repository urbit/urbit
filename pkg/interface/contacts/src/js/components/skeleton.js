import React, { Component } from 'react';
import classnames from 'classnames';

import { HeaderBar } from '/components/lib/header-bar';
import { GroupSidebar } from '/components/lib/group-sidebar';

export class Skeleton extends Component {
  render() {
    const { props } = this;
    let rightPanelClasses =
      props.activeDrawer === "groups" ? "dn flex-m flex-l flex-xl" : "flex";

    return (
      <div className="h-100 w-100 ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl">
        <HeaderBar spinner={props.spinner} />
        <div className="cf w-100 h-100 h-100-m-40-ns flex ba-m ba-l ba-xl b--gray2 br1">
          <GroupSidebar
            contacts={props.contacts}
            groups={props.groups}
            invites={props.invites}
            activeDrawer={props.activeDrawer}
            selected={props.selected}
            history={props.history}
            api={api}
          />
          <div
            className={"h-100 w-100 " + rightPanelClasses}
            style={{ flexGrow: 1 }}>
            {props.children}
          </div>
        </div>
      </div>
    );
  }
}
