import React, { Component } from 'react';

export class SidebarSwitcher extends Component {
  render() {

    const classes = this.props.classes ? this.props.classes : '';

    const style = this.props.style || {};

    const paddingTop = this.props.classes ? '0px' : '8px';

    return (
      <div className={classes} style={{ paddingTop: paddingTop, ...style }}>
        <a
          className='pointer flex-shrink-0'
          onClick={() => {
            this.props.api.local.sidebarToggle();
          }}
        >
          <img
            className='pr3 dn dib-m dib-l dib-xl'
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
