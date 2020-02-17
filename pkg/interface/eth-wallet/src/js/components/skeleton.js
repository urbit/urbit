import React, { Component } from 'react';
import classnames from 'classnames';

export class Skeleton extends Component {
  render() {

    let rightPanelHide = this.props.rightPanelHide
    ? "dn-s"
    : "";

    let popout = !!this.props.popout
    ? this.props.popout
    : false;

    let popoutWindow = (popout)
    ? ""
    : "h-100-m-40-ns ph4-m ph4-l ph4-xl pb4-m pb4-l pb4-xl"

    let popoutBorder = (popout)
    ? ""
    : "ba-m ba-l ba-xl b--gray2 br1"

    return (
      <div className={"h-100 w-100 " + popoutWindow}>
        <div className={`cf w-100 h-100 flex ` + popoutBorder}>
          <div className={"flex h-100 w-100 " + rightPanelHide} style={{
            flexGrow: 1,
          }}>
          <div className="w-50 h-100 dt br b--gray2">
            <div className="dtc h-100 v-mid f5 inter tc">1 ETH</div>
          </div>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
