import React, { Component } from 'react';
import classnames from 'classnames';

import { HeaderBar } from '/components/lib/header-bar.js';

export class Skeleton extends Component {
  render() {

    let sidebarHide = (!this.props.sidebarShown || this.props.popout)
    ? "dn"
    : "";

    let sidebarHideOnMobile = this.props.sidebarHideOnMobile
    ? "dn-s"
    : "";

    let chatHideOnMobile = this.props.chatHideonMobile
    ? "dn-s"
    : "";

    return (
      <div className="h-100 w-100 absolute">
        <HeaderBar spinner={this.props.spinner} popout={this.props.popout} />
        <div className={`cf w-100 absolute flex  ` +
          ((this.props.chatHideonMobile)
          ? "h-100 "
          : "h-100-minus-48-s ") +
          ((this.props.popout)
            ? "h-100"
            : "h-100-minus-48-m h-100-minus-48-l h-100-minus-48-xl")}>
          <div className={
              `fl h-100 br b--gray4 overflow-x-hidden
              flex-basis-full-s flex-basis-300-m flex-basis-300-l
              flex-basis-300-xl ` +
              sidebarHide + " " +
              sidebarHideOnMobile
            }>
            <div className={
                chatHideOnMobile === ""
                  ? "dn"
                  : "db dn-m dn-l dn-xl w-100 inter pt4 f8"
              }>
              <a className="pl3 pb6" href="/">
                {"⟵ Landscape"}
              </a>
              <div className="bb b--gray4 inter f8 pl3 pt6 pb3">All Chats</div>
            </div>
            {this.props.sidebar}
          </div>
          <div className={"h-100 fr " + chatHideOnMobile}
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
