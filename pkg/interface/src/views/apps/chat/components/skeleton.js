import React, { Component } from 'react';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import ErrorBoundary from '~/views/components/ErrorBoundary';

export class Skeleton extends Component {
  render() {
    // sidebar and chat panel conditional classes
    const sidebarHide = (!this.props.sidebarShown)
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

    return (
      // app outer skeleton
      <div className='h-100 w-100 ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl'>
      {/* app window borders */}
        <div className='bg-white bg-gray0-d cf w-100 flex h-100 ba-m ba-l ba-xl b--gray4 b--gray1-d br1'>
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
              <div className="bb b--gray4 b--gray1-d white-d inter f8 pl3 pb3">
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
            <ErrorBoundary>
              {this.props.children}
            </ErrorBoundary>
          </div>
        </div>
      </div>
    );
  }
}
