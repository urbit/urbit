import React, { Component } from 'react';
import classnames from 'classnames';

export class Skeleton extends Component {
  render() {
    // sidebar and chat panel conditional classes
    const sidebarHide = (!this.props.sidebarShown || this.props.popout)
      ? 'dn' : '';

    const sidebarHideOnMobile = this.props.sidebarHideOnMobile
      ? 'dn-s' : '';

    const chatHideOnMobile = this.props.chatHideonMobile
      ? 'dn-s' : '';

    // mobile-specific navigation classes
    const mobileNavClasses = classnames({
      'dn': this.props.chatHideOnMobile,
      'db dn-m dn-l dn-xl': !this.props.chatHideOnMobile,
      'w-100 inter pt4 f8': !this.props.chatHideOnMobile
    });

    // popout switches out window chrome and borders
    const popoutWindow = this.props.popout
      ? '' : 'ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl';

    const popoutBorder = this.props.popout
      ? '' : 'ba-m ba-l ba-xl b--gray4 b--gray1-d br1 ';

    return (
      // app outer skeleton
      <div style={{
        height: 'calc(100vh - 45px)'
      }} className={'h-100 w-100 bg-gray0-d ' + popoutWindow}
      >
      {/* app window borders */}
        <div className={ 'cf w-100 flex h-100 ' + popoutBorder }>
          {/* sidebar skeleton, hidden on mobile when in chat panel */}
          <div
            className={
              `fl h-100 br b--gray4 b--gray1-d overflow-x-hidden
              flex-basis-full-s flex-basis-250-m flex-basis-250-l
              flex-basis-250-xl ` +
              sidebarHide +
              ' ' +
              sidebarHideOnMobile
            }
          >
            {/* mobile-specific navigation */}
            <div className={mobileNavClasses}>
              <a className="pl3 pb6" href="/">
                {'⟵ Landscape'}
              </a>
              <div className="bb b--gray4 b--gray1-d white-d inter f8 pl3 pt6 pb3">
                All Chats
              </div>
            </div>
            {/* sidebar component inside the sidebar skeleton */}
            {this.props.sidebar}
          </div>
          {/* right-hand panel for chat, members, settings */}
          <div
            className={'h-100 fr ' + chatHideOnMobile}
            style={{
              flexGrow: 1,
              width: 'calc(100% - 300px)'
            }}
          >
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
