import React from 'react';
import { Col } from '@tlon/indigo-react';

import { EmptyGroupHome } from './EmptyGroupHome';
import { GroupFeed } from './GroupFeed';


export function GroupHome(props) {
  //  TODO: store a backend config for whether the feed is enabled or not
  const isFeedEnabled = false;

  return (
    <Col
      alignItems="center"
      justifyContent="center"
      display="flex"
      p='4'
    >
      { isFeedEnabled ? (
        <GroupFeed {...props} />
      ) : (
        <EmptyGroupHome {...props} />
      )}
    </Col>
  );
}
