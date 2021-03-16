import React from 'react';
import { Box, Row, Text } from '@tlon/indigo-react'
import { GroupFeedHeader } from './GroupFeedHeader';
import { PostInput } from './PostInput';
import { PostFeed } from './PostFeed';


export function GroupFeed(props) {
  const { baseUrl, api, history } = props;

  return (
    <Box
      width="100%"
      height="100%"
      display="flex"
      flexDirection="column"
      alignItems="center">
      <GroupFeedHeader baseUrl={baseUrl} history={history} />
      <Row width="100%" maxWidth="616px" pt="4" pl="" pr="2" flexGrow="1">
        <PostInput api={api} />
        <PostFeed />
      </Row> 
    </Box>
  );
}


