import React, { Component } from 'react'

export class Popout extends Component {
  render() {
    let hidePopoutIcon = this.props.popout
      ? "dn-m dn-l dn-xl"
      : "dib-m dib-l dib-xl";
    return (
      <div
        className="dib absolute z-2"
        style={{
          right: 16,
          top: 16
        }}>
        <a href="/~dojo/popout" target="_blank">
          <img
            className={`flex-shrink-0 dn ` + hidePopoutIcon}
            src="/~dojo/img/popout.png"
            height="16"
            width="16"
          />
        </a>
      </div>
    );
  }
}

export default Popout
