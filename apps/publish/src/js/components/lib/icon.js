import React, { Component } from 'react';
import { IconInbox } from '/components/lib/icons/icon-inbox';
import { IconComment } from '/components/lib/icons/icon-comment';
import { IconSig } from '/components/lib/icons/icon-sig';
import { IconDecline } from '/components/lib/icons/icon-decline';
import { IconUser } from '/components/lib/icons/icon-user';

export class Icon extends Component {
  render() {
    let iconElem = null;

    switch(this.props.type) {
      case "icon-stream-chat":
        iconElem = <span className="icon-stream-chat"></span>;
        break;
      case "icon-stream-dm":
        iconElem = <span className="icon-stream-dm"></span>;
        break;
      case "icon-collection-index":
        iconElem = <span className="icon-collection"></span>;
        break;
      case "icon-collection-post":
        iconElem = <span className="icon-collection-post"></span>;
        break;
      case "icon-collection-comment":
        iconElem = <span className="icon-collection icon-collection-comment"></span>;
        break;
      case "icon-panini":
        // TODO: Should icons be display: block, inline, or inline-blocks?
        //   1) Should naturally flow inline
        //   2) But can't make icon-panini naturally inline without hacks like &nbsp;
        iconElem = <div className="icon-panini"></div>
        break;
      case "icon-x":
        iconElem = <span className="icon-x"></span>
        break;
      case "icon-decline":
        iconElem = <IconDecline />
        break;
      case "icon-lus":
        iconElem = <span className="icon-lus"></span>
        break;
      case "icon-inbox":
        iconElem = <IconInbox />
        break;
      case "icon-comment":
        iconElem = <IconComment />
        break;
      case "icon-sig":
        iconElem = <IconSig />
        break;
      case "icon-user":
        iconElem = <IconUser />
        break;
      case "icon-ellipsis":
        iconElem = (
          <div className="icon-ellipsis-wrapper icon-label">
            <div className="icon-ellipsis-dot"></div>
            <div className="icon-ellipsis-dot"></div>
            <div className="icon-ellipsis-dot"></div>
          </div>
        )
        break;
    }

    let className = this.props.label ? "icon-label" : "";

    return (
      <span className={className}>
        {iconElem}
      </span>
    )
  }
}
