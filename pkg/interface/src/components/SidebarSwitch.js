import React, { Component } from 'react';

export class SidebarSwitcher extends Component {
  render() {
    const popoutSwitcher = this.props.popout
      ? ' dn-m dn-l dn-xl'
      : ' dib-m dib-l dib-xl';

    const classes = this.props.classes ? this.props.classes : '';

    const paddingTop = this.props.classes ? '0px' : '8px';

    return (
      <div className={classes} style={{ paddingTop: paddingTop }}>
        <a
          className='pointer flex-shrink-0'
          onClick={() => {
            this.props.api.local.sidebarToggle();
          }}
        >
          <img
            className={'pr3 dn ' + popoutSwitcher}
            src={
              this.props.sidebarShown
                ? '/~landscape/img/ChatSwitcherLink.png'
                : '/~landscape/img/ChatSwitcherClosed.png'
            }
            height='16'
            width='16'
            style={{
              maxWidth: 16
            }}
          />
        </a>
      </div>
    );
  }
}

export default SidebarSwitcher;
