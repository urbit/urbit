import React, { Component } from 'react';
import { ChannelItem } from './channel-item';

export class GroupItem extends Component {
  render() {
    const { props } = this;
    const association = props.association ? props.association : {};

    let title = association['app-path'] ? association['app-path'] : 'Direct Messages';
    if (association.metadata && association.metadata.title) {
      title = association.metadata.title !== ''
        ? association.metadata.title
        : title;
    }

    const channels = props.channels ? props.channels : [];
    const first = (props.index === 0) ? 'pt1' : 'pt4';

    const channelItems = channels.sort((a, b) => {
      if (props.index === '/~/') {
        const aPreview = props.messagePreviews[a];
        const bPreview = props.messagePreviews[b];
        const aWhen = aPreview ? aPreview.when : 0;
        const bWhen = bPreview ? bPreview.when : 0;

        return bWhen - aWhen;
      } else {
      const aAssociation = a in props.chatMetadata ? props.chatMetadata[a] : {};
      const bAssociation = b in props.chatMetadata ? props.chatMetadata[b] : {};
      let aTitle = a;
      let bTitle = b;
      if (aAssociation.metadata && aAssociation.metadata.title) {
        aTitle = (aAssociation.metadata.title !== '')
          ? aAssociation.metadata.title : a;
        }
      if (bAssociation.metadata && bAssociation.metadata.title) {
        bTitle =
          bAssociation.metadata.title !== '' ? bAssociation.metadata.title : b;
        }
      return aTitle.toLowerCase().localeCompare(bTitle.toLowerCase());
      }
    }).map((each, i) => {
      const unread = props.unreads[each];
      let title = each.substr(1);
      if (
        each in props.chatMetadata &&
        props.chatMetadata[each].metadata
      ) {
        title = props.chatMetadata[each].metadata.title
          ? props.chatMetadata[each].metadata.title
          : each.substr(1);
      }
      const selected = props.station === each;

      return (
        <ChannelItem
          key={i}
          unread={unread}
          title={title}
          selected={selected}
          box={each}
          {...props}
        />
      );
    });
    return (
      <div className={first}>
      <p className="f9 ph4 fw6 pb2 gray3">{title}</p>
      {channelItems}
      </div>
    );
  }
}

export default GroupItem;
