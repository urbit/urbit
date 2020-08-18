import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { ChannelItem } from './channel-item';
import { Text } from '@tlon/indigo-react';

export class GroupItem extends Component {
  dmLink(index) {
    if (index === 'dm') {
      return (
        <Link
          className='absolute right-0 f9 top-0 mr4 green2 bg-gray5 bg-gray1-d b--transparent br1'
          to='/~chat/new/dm'
          style={{ padding: '0rem 0.2rem' }}
        >
          + DM
        </Link>
      );
    } else {
      return <div />;
    };
  }

  getTitle(each, props) {
    const metadata = props.chatMetadata?.[each]?.metadata;
    if (props.index === 'dm' && !(metadata && !metadata.title.includes('<->'))) {
      const group = props.chatMetadata?.[each]?.['group-path'];
      if (props.groups?.[group]?.members) {
        let title = Array.from(props.groups?.[group]?.members).filter((e) => {
          return e !== window.ship;
        });
        if (title.length > 3) {
          title = title.slice(3).push(`and ${title.length - 3} others`);
        } else if (title.length === 0) {
          return `~${window.ship}`;
        } else if (title.length === 1) {
          return title.join('');
        }
        return title.join(', ');
      }
    } else if (metadata) {
      return metadata.title ? metadata.title : each.substr(1);
    } else {
      return each.substr(1);
    }
  }

  sort(channels, props) {
    return channels.sort((a, b) => {
      if (props.index === 'dm') {
        const aPreview = props.messagePreviews[a];
        const bPreview = props.messagePreviews[b];
        const aWhen = aPreview ? aPreview.when : 0;
        const bWhen = bPreview ? bPreview.when : 0;

        return bWhen - aWhen;
      } else {
        const aAssociation =
          a in props.chatMetadata ? props.chatMetadata[a] : {};
        const bAssociation =
          b in props.chatMetadata ? props.chatMetadata[b] : {};
        let aTitle = a;
        let bTitle = b;
        if (aAssociation.metadata && aAssociation.metadata.title) {
          aTitle =
            aAssociation.metadata.title !== ''
              ? aAssociation.metadata.title
              : a;
        }
        if (bAssociation.metadata && bAssociation.metadata.title) {
          bTitle =
            bAssociation.metadata.title !== ''
              ? bAssociation.metadata.title
              : b;
        }
        return aTitle.toLowerCase().localeCompare(bTitle.toLowerCase());
      }
    });
  }

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

    const channelItems = this.sort(channels, props).map((each, i) => {
      const unread = props.unreads[each];
      const title = this.getTitle(each, props);
      const selected = props.station === each;

      return (
        <ChannelItem
          key={i}
          unread={unread}
          title={title}
          selected={selected}
          dm={Boolean(props.index === 'dm')}
          box={each}
          {...props}
        />
      );
    });

    if (channelItems.length === 0) {
      channelItems.push(
      <Text display='block' gray mt='4' textAlign='center'>
        No direct messages
      </Text>);
    }

    const dmLink = this.dmLink(props.index);

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
