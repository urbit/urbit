import React, { Component } from 'react';
import classnames from 'classnames';

import { HeaderBar } from '/components/lib/header-bar';

export class Skeleton extends Component {
  render() {
    const { props } = this;
    let rightPanelClasses =
      props.activeDrawer === "keybase" ? "dn flex-m flex-l flex-xl" : "flex";

    return (
      <div className="h-100 w-100 ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl">
        <HeaderBar />
        <div className="cf w-100 h-100 h-100-m-40-ns flex ba-m ba-l ba-xl b--gray4 b--gray1-d br1">
          <BitcoinSidebar
            activeDrawer={props.activeDrawer}
            history={props.history}
            api={api}
            selected={this.props.selected}
          />
            <div className="h-100 w-100 flex-auto relative" style={{
              flexGrow: 1,
            }}>
            {props.children}
          </div>
        </div>
      </div>
    );
  }
}
