import { Box, Col, Text } from '@tlon/indigo-react';
import bigInt from 'big-integer';
import React from 'react';
import { resourceFromPath } from '~/logic/lib/group';
import { Loading } from '~/views/components/Loading';
import PostFeed from './PostFeed';
import PostItem from './PostItem/PostItem';

export default function PostThread(props) {
  const {
    baseUrl,
    api,
    history,
    association,
    graphPath,
    group,
    vip,
    pendingSize
  } = props;

  //  TODO: make thread
  return (
    <Box></Box>
  );

}

