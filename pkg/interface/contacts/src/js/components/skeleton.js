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
      <div className="h-100 w-100">
        <HeaderBar spinner={props.spinner} />
        <div className="cf w-100 h-100 h-100-m-48-ns flex">
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
