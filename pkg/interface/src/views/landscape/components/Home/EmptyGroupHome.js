import React from 'react';
import { Col, Box, Text } from '@tlon/indigo-react';

import { GroupSummary } from '../GroupSummary';


export function EmptyGroupHome(props) {
  const { groupPath, associations, groups } = props;
  const groupAssociation =
    (groupPath && associations.groups[groupPath]) || undefined;

  const hasDescription = groupAssociation?.metadata?.description;
  const channelCount = Object.keys(associations?.graph ?? {}).filter((e) => {
    return associations?.graph?.[e]?.['group'] === groupPath;
  }).length;

  return (
    <Col
      width="100%"
      height="100%"
      justifyContent="center"
      alignItems="center"
      display="flex">
      { groupAssociation?.group ? (
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
      ) }
    </Col>
  );

}

