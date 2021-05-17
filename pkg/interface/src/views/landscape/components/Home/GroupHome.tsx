import { Box } from '@tlon/indigo-react';
import { GroupConfig, resourceFromPath } from '@urbit/api';
import React from 'react';
import { Route } from 'react-router-dom';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';
import { AddFeedBanner } from './AddFeedBanner';
import { EmptyGroupHome } from './EmptyGroupHome';
import { EnableGroupFeed } from './EnableGroupFeed';
import { GroupFeed } from './GroupFeed';
import {getFeedPath} from '~/logic/lib/util';

function GroupHome(props) {
  const {
    api,
    groupPath,
    baseUrl
  } = props;

  const { ship } = resourceFromPath(groupPath);

  const associations = useMetadataState(state => state.associations);
  const groups = useGroupState(state => state.groups);

  const association = associations?.groups[groupPath];

  const feedPath = getFeedPath(association);

  const askFeedBanner = feedPath === undefined;

  const isFeedEnabled = !!feedPath;

  const graphMetadata = associations?.graph[feedPath]?.metadata;

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
          baseUrl={baseUrl}
          group={groups[groupPath]}
        />
      ) : null }
      <Route path={`${baseUrl}/feed`}>
        <GroupFeed
          graphPath={feedPath}
          groupPath={groupPath}
          vip={graphMetadata?.vip || ''}
          api={api}
          baseUrl={baseUrl}
        />
      </Route>
      <Route path={baseUrl} exact>
        <EmptyGroupHome
          groups={groups}
          associations={associations}
          groupPath={groupPath}
        />
      </Route>
    </Box>
  );
}

export { GroupHome };
