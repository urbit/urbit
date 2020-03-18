import React, { Component } from 'react';
import classnames from 'classnames';
import { Sigil } from '/components/lib/icons/sigil';
import { uxToHex, cite } from '/lib/util';


export class MemberElement extends Component {

  onRemove() {
    const { props } = this;
    //TODO don't really need to use link-view here, but should we anyway?
    api.groups.remove(props.groupPath, [`~${props.ship}`]);
  }

  render() {
    const { props } = this;

    let actionElem;
    if (props.ship === props.owner) {
      actionElem = (
        <p className="w-20 dib list-ship black white-d f8 c-default">
          Host
        </p>
      );
    } else if (props.amOwner && window.ship !== props.ship) {
      actionElem = (
        <a onClick={this.onRemove.bind(this)}
           className="w-20 dib list-ship black white-d f8 pointer">
           Ban
        </a>
      );
    } else {
      actionElem = (
        <span></span>
      );
    }

    let name = !!props.contact
      ? `${props.contact.nickname} (${cite(props.ship)})`
      : `${cite(props.ship)}`;
    let color = !!props.contact ? uxToHex(props.contact.color) : '000000';

    return (
      <div className="flex mb2">
        <Sigil ship={props.ship} size={32} color={`#${color}`} />
        <p className={"w-70 mono list-ship dib v-mid black white-d ml2 nowrap f8"}
           title={props.ship}>
            {name}
           </p>
        {actionElem}
      </div>
    );
  }
}
