import React, { ReactElement, useRef } from 'react';
import { Col, Box, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';

import { GroupSummary } from './GroupSummary';


export function EmptyGroupHome(props) {
  const { groupPath, associations, groups } = props;
  const groupAssociation =
    (groupPath && associations.groups[groupPath]) || undefined;

  const hasDescription = groupAssociation?.metadata?.description;
  const channelCount = Object.keys(associations?.graph ?? {}).filter((e) => {
    return associations?.graph?.[e]?.['group'] === groupPath;
  }).length;

  return groupAssociation?.group ? (
    <GroupSummary
      memberCount={groups[groupAssociation.group].members.size}
      channelCount={channelCount}
      metadata={groupAssociation.metadata}
      resource={groupAssociation.group}
    />
  ) : (
    <Box p="4">
      <Text color='gray'>
        Create or select a channel to get started
      </Text>
    </Box>
  );
}

export function GroupFeed(props) {
  const { baseUrl } = props;
  return (
    <>
      <Link to={baseUrl}><Text>{'<- Back'}</Text></Link>
    </>
  );
}

export function GroupHome(props) {
  //  TODO: store a backend config for whether the feed is enabled or not
  const isFeedEnabled = true;

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
