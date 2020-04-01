import React, { Component } from 'react';
import { ChannelItem } from './channel-item';

export class GroupItem extends Component {
  render() {
    const { props, state } = this;
    let association = !!props.association ? props.association : {};

    let title = association["app-path"] ? association["app-path"] : "Direct Messages";
    if (association.metadata && association.metadata.title) {
      title = association.metadata.title !== ""
        ? association.metadata.title
        : title;
    }

    let channels = !!props.channels ? props.channels : [];
    let first = (props.index === 0) ? "pt1" : "pt4"


    let channelItems = channels.sort((a, b) => {
      if (props.index === "/~/") {
        let aPreview = props.messagePreviews[a];
        let bPreview = props.messagePreviews[b];
        let aWhen = !!aPreview ? aPreview.when : 0;
        let bWhen = !!bPreview ? bPreview.when : 0;

        return bWhen - aWhen;
      } else {
      let aAssociation = a in props.chatMetadata ? props.chatMetadata[a] : {};
      let bAssociation = b in props.chatMetadata ? props.chatMetadata[b] : {};
      let aTitle = a;
      let bTitle = b;
      if (aAssociation.metadata && aAssociation.metadata.title) {
        aTitle = (aAssociation.metadata.title !== "")
          ? aAssociation.metadata.title : a;
        }
      if (bAssociation.metadata && bAssociation.metadata.title) {
        bTitle =
          bAssociation.metadata.title !== "" ? bAssociation.metadata.title : b;
        }
      return aTitle.toLowerCase().localeCompare(bTitle.toLowerCase());
      }
    }).map((each, i) => {
      let unread = props.unreads[each];
      let title = each.substr(1);
      if (
        each in props.chatMetadata &&
        props.chatMetadata[each].metadata
      ) {
        title = props.chatMetadata[each].metadata.title
          ? props.chatMetadata[each].metadata.title
          : each.substr(1);
      }
      let selected = props.station === each;

      return (
        <ChannelItem
          key={i}
          unread={unread}
          title={title}
          selected={selected}
          box={each}
          {...props}
        />
      )
    })
    return (
      <div className={first}>
      <p className="f9 ph4 fw6 pb2 gray3">{title}</p>
      {channelItems}
      </div>
    )
  }
}

export default GroupItem
