import React from 'react';
import { Box, Col } from '@tlon/indigo-react';

import { EmptyGroupHome } from './EmptyGroupHome';
import { GroupFeed } from './GroupFeed';
import { AddFeedBanner } from './AddFeedBanner';


export function GroupHome(props) {
  const {
    associations,
    api,
    groupPath,
    groups
  } = props;

  const metadata = associations?.groups[groupPath]?.metadata;
  const askFeedBanner =
    metadata &&
    metadata.config &&
    'group' in metadata.config &&
    metadata.config.group === null;
  const isFeedEnabled =
    metadata &&
    metadata.config &&
    metadata.config.group &&
    'resource' in metadata.config.group;

  return (
    <Box width="100%" height="100%">
      { askFeedBanner ? (
        <AddFeedBanner 
          api={api}
          groupPath={groupPath}
          group={groups[groupPath]}
        /> 
      ) : null }
      { isFeedEnabled ? (
        <GroupFeed {...props} />
      ) : (
        <EmptyGroupHome {...props} />
      )}
    </Box>
  );
}
