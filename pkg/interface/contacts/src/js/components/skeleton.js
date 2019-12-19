import React, { Component } from 'react';
import classnames from 'classnames';

import { HeaderBar } from './lib/header-bar';
import { GroupSidebar } from './lib/group-sidebar';

export class Skeleton extends Component {
  render() {
    let rightPanelClasses =
      this.props.activeDrawer === "groups" ?
      "dn flex-m flex-l flex-xl" : "flex";

    return (
      <div className="h-100 w-100">
        <HeaderBar spinner={this.props.spinner} />
        <div className="cf w-100 h-100 h-100-m-48-ns flex">
          <GroupSidebar
            contacts={this.props.contacts} 
            activeDrawer={this.props.activeDrawer}
            selected={this.props.selected}/>
          <div
            className={"h-100 w-100 " + rightPanelClasses}
            style={{ flexGrow: 1 }}>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
