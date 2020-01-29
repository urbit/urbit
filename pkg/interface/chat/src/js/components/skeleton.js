import React, { Component } from 'react';
import classnames from 'classnames';
import { HeaderBar } from './lib/header-bar';

export class Skeleton extends Component {
  render() {

    // sidebar and chat panel conditional classes
    let sidebarHide = (!this.props.sidebarShown || this.props.popout)
    ? "dn"
    : "";

    let sidebarHideOnMobile = this.props.sidebarHideOnMobile
    ? "dn-s"
    : "";

    let chatHideOnMobile = this.props.chatHideonMobile
    ? "dn-s"
    : "";

    // mobile-specific navigation classes
    let mobileNavClasses = classnames({
      "dn": this.props.chatHideOnMobile,
      "db dn-m dn-l dn-xl": !this.props.chatHideOnMobile,
      "w-100 inter pt4 f8": !this.props.chatHideOnMobile
    })

    // popout switches out window chrome and borders
    let popoutWindow = this.props.popout
    ? ""
    : "ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl";

    let popoutBorder = this.props.popout 
    ? "" 
    : "ba-m ba-l ba-xl b--gray2 br1 ";

    return (
      // app outer skeleton
      <div className={"absolute h-100 w-100 bg-gray0-d " + popoutWindow}>
      <HeaderBar spinner={this.props.spinner} />
      {/* app window borders */}
        <div className={
            `cf w-100 flex ` +
            popoutBorder +
            (this.props.chatHideonMobile ? "h-100 " : "h-100-minus-40-s ") +
            (this.props.popout
              ? "h-100"
              : "h-100-minus-40-m h-100-minus-40-l h-100-minus-40-xl")
          }>
          {/* sidebar skeleton, hidden on mobile when in chat panel */}
          <div
            className={
              `fl h-100 br b--gray4 b--gray1-d overflow-x-hidden
              flex-basis-full-s flex-basis-300-m flex-basis-300-l
              flex-basis-300-xl ` +
              sidebarHide +
              " " +
              sidebarHideOnMobile
            }>
            {/* mobile-specific navigation */}
            <div className={mobileNavClasses}>
              <a className="pl3 pb6" href="/">
                {"⟵ Landscape"}
              </a>
              <div className="bb b--gray4 white-d inter f8 pl3 pt6 pb3">
                All Chats
              </div>
            </div>
            {/* sidebar component inside the sidebar skeleton */}
            {this.props.sidebar}
          </div>
          {/* right-hand panel for chat, members, settings */}
          <div
            className={"h-100 fr " + chatHideOnMobile}
            style={{
              flexGrow: 1,
              width: "calc(100% - 300px)"
            }}>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
