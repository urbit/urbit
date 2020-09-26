import React, { Component } from 'react';
import { ChannelItem } from './channel-item';
import { deSig } from '~/logic/lib/util';


export const GroupItem = (props) => {
  const association = props.association ? props.association : {};

  let title =
    association['app-path'] ? association['app-path'] : 'Unmanaged Collections';

  if (association.metadata && association.metadata.title) {
    title = association.metadata.title !== ''
      ? association.metadata.title : title;
  }

  const channels = props.channels ? props.channels : [];
  const unmanaged = props.unmanaged ? 'pt6' : 'pt1';

  const channelItems = channels.map((each, i) => {
    const meta = props.metadata[each];
    if (!meta) { return null; }
    const link = `${deSig(each.split('/')[2])}/${each.split('/')[3]}`;

    const selected = (props.selected === each);
    return (
      <ChannelItem
        key={each}
        link={link}
        selected={selected}
        name={meta.metadata.title}
      />
    );
  });

  return (
    <div className={unmanaged}>
      <p className="f9 ph4 pb2 gray3">{title}</p>
      {channelItems}
    </div>
  );
};

