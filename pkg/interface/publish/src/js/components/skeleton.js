import React, { Component } from 'react';
import { HeaderBar } from './lib/header-bar';
import { Sidebar } from './lib/sidebar';

export class Skeleton extends Component {
  render() {
    const { props, state } = this;

    let rightPanelHide = props.rightPanelHide
      ? "dn-s" : "";

    let popout = !!props.popout
      ? props.popout : false;

    let popoutWindow = (popout)
      ? "" : "h-100-m-40-ns ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl"

    let popoutBorder = (popout)
      ? "": "ba-m ba-l ba-xl b--gray4 b--gray1-d br1"

    return (
      <div className={"absolute h-100 w-100 " + popoutWindow}>
      <HeaderBar
        invites={props.invites}
        associations={props.associations} />
        <div className={`cf w-100 h-100 flex ` + popoutBorder}>
          <Sidebar
            popout={popout}
            sidebarShown={props.sidebarShown}
            active={props.active}
            notebooks={props.notebooks}
            contacts={props.contacts}
            path={props.path}
            invites={props.invites}
            associations={props.associations}
            selectedGroups={props.selectedGroups}
            />
          <div className={"h-100 w-100 relative white-d flex-auto " + rightPanelHide} style={{
            flexGrow: 1,
          }}>
            {props.children}
          </div>
        </div>
      </div>
    );
  }
}

export default Skeleton;
