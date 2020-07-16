import React, { Component } from 'react';
import { Link } from 'react-router-dom';
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
    const first = (props.index === 0) ? 'mt1 ' : 'mt6 ';

    const channelItems = channels.sort((a, b) => {
      if (props.index === 'dm') {
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

    if (channelItems.length === 0) {
      channelItems.push(<p className="gray2 mt4 f9 tc">No direct messages</p>);
    }

    let dmLink = <div />;

    if (props.index === 'dm') {
      dmLink = <Link
                className="absolute right-0 f9 top-0 mr4 green2 bg-gray5 bg-gray1-d b--transparent br1"
                to="/~chat/new/dm"
                style={{ padding: '0rem 0.2rem' }}
               >
               + DM
               </Link>;
    }
    return (
      <div className={first + 'relative'}>
      <p className="f9 ph4 gray3">{title}</p>
        {dmLink}
        {channelItems}
      </div>
    );
  }
}

export default GroupItem;
