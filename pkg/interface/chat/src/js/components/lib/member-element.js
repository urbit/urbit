import React, { Component } from 'react';
import classnames from 'classnames';


export class MemberElement extends Component {

  onRemove() {
    const { props } = this;
    props.api.unpermit(props.circle, props.ship);
  }

  render() {
    const { props } = this;

    let actionElem;
    if (`~${props.ship}` === props.host) {
      actionElem = (
        <p className="dib w-40 underline black label-small-mono label-regular">
          Host
        </p>
      );
    } else if (window.ship !== props.ship &&
      `~${window.ship}` === props.host) {
      actionElem = (
        <a onClick={this.onRemove.bind(this)}
           className="w-40 dib underline black btn-font">
           Remove
        </a>
      );
    } else {
      actionElem = (
        <span></span>
      );
    }

    return (
      <div>
        <p
          className={
            "w-60 dib black pr3 mb2 label-small-mono label-regular"
          }>
          {props.ship}
        </p>
        {actionElem}
      </div>
    );
  }
}


