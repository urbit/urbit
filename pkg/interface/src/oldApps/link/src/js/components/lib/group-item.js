import React, { Component } from 'react';
import { ChannelsItem } from './channels-item';

export class GroupItem extends Component {
  render() {
    const { props, state } = this;
    let association = !!props.association ? props.association : {};

    let title = association["app-path"] ? association["app-path"] : "Unmanaged Collections";

    if (association.metadata && association.metadata.title) {
      title = association.metadata.title !== ""
        ? association.metadata.title : title;
    }

    let channels = !!props.channels ? props.channels : [];
    let first = (props.index === 0) ? "pt1" : "pt4";

    let channelItems = channels.map((each, i) => {
      const meta = props.linkMetadata[each];
      if (!meta) return null;
      const selected = (props.selected === each);
      const unseenCount = !!props.links[each]
        ? props.links[each].unseenCount
        : 0;
        return (
          <ChannelsItem
            key={each}
            link={each}
            selected={selected}
            unseenCount={unseenCount}
            name={meta.metadata.title}
          />
        )
    })
    return (
      <div className={first}>
      <p className="f9 ph4 pb2 fw6 gray3">{title}</p>
        {channelItems}
      </div>
    )
  }
}

export default GroupItem;