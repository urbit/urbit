import React, { Component } from 'react';
import classnames from 'classnames';

import { HeaderBar } from './lib/header-bar';
import { Groups } from './groups';

export class Skeleton extends Component {
  render() {
      let rightPanelClasses =
        this.props.activeDrawer === "groups" ? "dn db-ns" : "db";

    return (
      <div className="h-100 w-100">
        <HeaderBar spinner={this.props.spinner} />
        <div
          className="cf w-100 flex"
          style={{
            height: "calc(100% - 48px)"
          }}>
          <Groups contacts={this.props.contacts} 
          activeDrawer={this.props.activeDrawer}
          selected={this.props.selected}/>
          <div
            className={"h-100 w-100 flex " + rightPanelClasses}
            style={{
              flexGrow: 1
            }}>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
