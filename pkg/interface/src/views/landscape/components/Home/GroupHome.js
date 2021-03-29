import React from 'react';
import { Box } from '@tlon/indigo-react';

import { EnableGroupFeed } from './EnableGroupFeed';
import { EmptyGroupHome } from './EmptyGroupHome';
import { GroupFeed } from './GroupFeed';
import { AddFeedBanner } from './AddFeedBanner';
import { Route } from 'react-router-dom';

import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';


function GroupHome(props) {
  const {
    api,
    groupPath,
    baseUrl
  } = props;

  const associations = useMetadataState(state => state.associations);
  const groups = useGroupState(state => state.groups);

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

  const graphPath = metadata?.config?.group?.resource;

  return (
    <Box width="100%" height="100%" overflow="hidden">
      <Route path={`${baseUrl}/enable`}
        render={() => {
          return (
            <EnableGroupFeed
              groupPath={groupPath}
              baseUrl={baseUrl}
              api={api}
            />
          );
        }}
      />
      { askFeedBanner ? (
        <AddFeedBanner 
          api={api}
          groupPath={groupPath}
          group={groups[groupPath]}
        /> 
      ) : null }
      { isFeedEnabled ? (
        <GroupFeed
          graphPath={graphPath}
          api={api}
          baseUrl={baseUrl} />
      ) : (
        <EmptyGroupHome
          groups={groups}
          associations={associations}
          groupPath={groupPath} />
      )}
    </Box>
  );
}

export { GroupHome };
