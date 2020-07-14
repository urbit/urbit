import React, { Component, useState } from 'react';
import classnames from 'classnames';
import { Row, Col, ColumnView, Sidebar, SidebarButton, Icon } from '@tlon/indigo-react'

const Skeleton = ({sidebar, children}) => {
    // sidebar and chat panel conditional classes
    // const sidebarHide = (!this.props.sidebarShown || this.props.popout)
    //   ? 'dn' : '';

    // const sidebarHideOnMobile = this.props.sidebarHideOnMobile
    //   ? 'dn-s' : '';

    // const chatHideOnMobile = this.props.chatHideonMobile
    //   ? 'dn-s' : '';

    // // mobile-specific navigation classes
    // const mobileNavClasses = classnames({
    //   'dn': this.props.chatHideOnMobile,
    //   'db dn-m dn-l dn-xl': !this.props.chatHideOnMobile,
    //   'w-100 inter pt4 f8': !this.props.chatHideOnMobile
    // });
    //

    const [open, setOpen] = useState(true);

    return (
        <Row height='100%'>
          <ColumnView open={open} onChange={() => setOpen(!open)}>
            <Sidebar>
              {sidebar}
            </Sidebar>
            <Col height='100%' width='100%'>
              {children}
            </Col>
          </ColumnView>
          
          {
          // {/* sidebar skeleton, hidden on mobile when in chat panel */}
          // <div
          //   className={
          //     `fl h-100 br b--gray4 b--gray1-d overflow-x-hidden
          //     flex-basis-full-s flex-basis-250-m flex-basis-250-l
          //     flex-basis-250-xl ` +
          //     sidebarHide +
          //     ' ' +
          //     sidebarHideOnMobile
          //   }
          // >
          //   {/* mobile-specific navigation */}
          //   <div className={mobileNavClasses}>
          //     <div className="bb b--gray4 b--gray1-d white-d inter f8 pl3 pb3">
          //       All Chats
          //     </div>
          //   </div>
          //   {/* sidebar component inside the sidebar skeleton */}
          //   {this.props.sidebar}
          // </div>
          // {/* right-hand panel for chat, members, settings */}
          // <div
          //   className={'h-100 fr ' + chatHideOnMobile}
          //   style={{
          //     flexGrow: 1,
          //     width: 'calc(100% - 300px)'
          //   }}
          // >
          //   {this.props.children}
          // </div>
      }
        </Row>
    );
}

export {Skeleton}
