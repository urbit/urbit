import React, { Component } from 'react';
import classnames from 'classnames';
import { Sigil } from '/components/lib/icons/sigil';


export class MemberElement extends Component {

  onRemove() {
    const { props } = this;
    props.api.groups.remove([`~${props.ship}`], props.path);
  }

  render() {
    const { props } = this;

    let actionElem;
    if (props.ship === props.owner) {
      actionElem = (
        <p className="dib w-20 underline black label-small-mono label-regular">
          Host
        </p>
      );
    } else if (window.ship !== props.ship && window.ship === props.owner) {
      actionElem = (
        <a onClick={this.onRemove.bind(this)}
           className="w-20 dib list-ship black underline label-small-mono pointer">
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
            "w-70 dib v-mid black ml2 nowrap label-small-mono list-ship label-regular"
          }>
          {props.ship}
        </p>
        {actionElem}
      </div>
    );
  }
}


