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
      ? "" : "ba-m ba-l ba-xl b--gray4 b--gray1-d br1"

      let linkInvites = ('/link' in this.props.invites)
      ? this.props.invites['/link'] : {};

    return (
      <div className={"absolute h-100 w-100 " + popoutWindow}>
        <HeaderBar
          invites={this.props.invites}
          associations={this.props.associations}
        />
        <div className={`cf w-100 h-100 flex ` + popoutBorder}>
        <ChannelsSidebar
            active={this.props.active}
            popout={popout}
            associations={this.props.associations}
            invites={linkInvites}
            groups={this.props.groups}
            selected={this.props.selected}
            selectedGroups={this.props.selectedGroups}
            sidebarShown={this.props.sidebarShown}
            links={this.props.links}
            listening={this.props.listening}/>
          <div className={"h-100 w-100 flex-auto relative " + rightPanelHide} style={{
            flexGrow: 1,
          }}>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
