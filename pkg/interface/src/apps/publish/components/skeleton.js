import React, { Component } from 'react';
import { Sidebar } from './lib/sidebar';

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
      ? '': 'ba-m ba-l ba-xl b--gray4 b--gray1-d br1';

    return (
      <div className={'w-100 h-100 ' + popoutWindow}>
        <div className={'cf w-100 h-100 flex ' + popoutBorder}>
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
            api={this.props.api}
          />
          <div className={'h-100 w-100 relative white-d flex-auto ' + rightPanelHide} style={{
            flexGrow: 1
          }}
          >
            {props.children}
          </div>
        </div>
      </div>
    );
  }
}

export default Skeleton;
