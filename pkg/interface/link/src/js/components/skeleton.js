import React, { Component } from 'react';
import classnames from 'classnames';
import { ChannelsSidebar } from './lib/channel-sidebar';


export class Skeleton extends Component {
  render() {

    let rightPanelHide = this.props.rightPanelHide
    ? "dn-s"
    : "";

    let popout = !!this.props.popout 
    ? this.props.popout 
    : false;

    let popoutWindow = (popout)
    ? ""
    : "h-100-m-40-ns ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl"

    let popoutBorder = (popout)
    ? ""
    : "ba-m ba-l ba-xl b--gray2 br1"

    return (
      <div className={"h-100 w-100 " + popoutWindow}>
        <div className={`cf w-100 h-100 flex ` + popoutBorder}>
        <ChannelsSidebar
            popout={popout}
            paths={this.props.paths} 
            active={this.props.active}
            selected={this.props.selected}
            sidebarShown={this.props.sidebarShown}
            links={this.props.links}/>
          <div className={"h-100 w-100 " + rightPanelHide} style={{
            flexGrow: 1,
          }}>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
