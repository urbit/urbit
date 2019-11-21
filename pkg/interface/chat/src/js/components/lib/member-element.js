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
        <p className="w-20 dib list-ship black f8 c-default">
          Host
        </p>
      );
    } else if (window.ship !== props.ship && window.ship === props.owner) {
      actionElem = (
        <a onClick={this.onRemove.bind(this)}
           className="w-20 dib list-ship black f8 pointer">
           Ban
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
            "w-70 mono list-ship dib v-mid black ml2 nowrap f8"
          }>
          ~{props.ship}
        </p>
        {actionElem}
      </div>
    );
  }
}


