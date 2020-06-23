import React, { Component } from 'react';
import { ChannelsSidebar } from './lib/channel-sidebar';

export class Skeleton extends Component {
  render() {
    const { props } = this;

    const rightPanelHide = props.rightPanelHide
      ? 'dn-s' : '';

    const popout = props.popout
      ? props.popout : false;

    const popoutWindow = (popout)
      ? '' : 'ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl';

    const popoutBorder = (popout)
      ? '' : 'ba-m ba-l ba-xl b--gray4 b--gray1-d br1';

      const linkInvites = ('/link' in props.invites)
      ? props.invites['/link'] : {};

    return (
      <div className={'absolute w-100 ' + popoutWindow} style={{ height: 'calc(100% - 45px)' }}>
        <div className={'cf w-100 h-100 flex ' + popoutBorder}>
        <ChannelsSidebar
            active={props.active}
            popout={popout}
            associations={props.associations}
            invites={linkInvites}
            groups={props.groups}
            selected={props.selected}
            selectedGroups={props.selectedGroups}
            sidebarShown={props.sidebarShown}
            links={props.links}
            listening={props.listening}
            api={props.api}
        />
          <div className={'h-100 w-100 flex-auto relative ' + rightPanelHide} style={{
            flexGrow: 1
          }}
          >
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
