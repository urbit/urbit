import React from 'react';
import { Col } from '@tlon/indigo-react';

import { EmptyGroupHome } from './EmptyGroupHome';
import { GroupFeed } from './GroupFeed';
import { AddFeedBanner } from './AddFeedBanner';


export function GroupHome(props) {
  //  TODO: store a backend config for whether the feed is enabled or not
  console.log(props);
  const metadata = props.associations?.groups[props.groupPath]?.metadata;
  console.log(metadata);
  const askFeedBanner = metadata && metadata.module === null;
  const isFeedEnabled = false;

  return (
    <Col
      alignItems="center"
      justifyContent="center"
      display="flex"
      p='4'
    >
      { askFeedBanner ? (
        <AddFeedBanner 
          api={props.api}
          groupPath={props.groupPath}
          group={props.groups[props.groupPath]}
        /> 
      ) : null }
      { isFeedEnabled ? (
        <GroupFeed {...props} />
      ) : (
        <EmptyGroupHome {...props} />
      )}
    </Col>
  );
}
