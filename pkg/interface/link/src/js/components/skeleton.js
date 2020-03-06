import React, { Component } from 'react';
import classnames from 'classnames';
import { HeaderBar } from './lib/header-bar';
import { ChannelsSidebar } from './lib/channel-sidebar';


export class Skeleton extends Component {
  render() {

    let rightPanelHide = this.props.rightPanelHide
      ? "dn-s" : "";

    let popout = !!this.props.popout
      ? this.props.popout : false;

    let popoutWindow = (popout)
      ? "" : "h-100-m-40-ns ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl"

    let popoutBorder = (popout)
      ? "" : "ba-m ba-l ba-xl b--gray2 br1"

    return (
      <div className={"absolute h-100 w-100 " + popoutWindow}>
      <HeaderBar spinner={this.props.spinner} />
        <div className={`cf w-100 h-100 flex ` + popoutBorder}>
        <ChannelsSidebar
            active={this.props.active}
            popout={popout}
            resources={this.props.resources}
            invites={this.props.invites}
            groups={this.props.groups}
            selected={this.props.selected}
            sidebarShown={this.props.sidebarShown}
            links={this.props.links}/>
          <div className={"h-100 w-100 flex-auto" + rightPanelHide} style={{
            flexGrow: 1,
          }}>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
