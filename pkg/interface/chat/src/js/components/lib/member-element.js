import React, { Component } from 'react';
import classnames from 'classnames';
import { Sigil } from '/components/lib/icons/sigil';


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
           className="w-40 dib list-ship black underline label-small-mono pointer">
           Remove
        </a>
      );
    } else {
      actionElem = (
        <span></span>
      );
    }

    return (
      <div className="flex mb2">
        <Sigil ship={props.ship} size={32} />
        <p
          className={
            "w-60 dib v-mid black pr3 ml2 nowrap label-small-mono list-ship label-regular"
          }>
          {props.ship}
        </p>
        {actionElem}
      </div>
    );
  }
}


