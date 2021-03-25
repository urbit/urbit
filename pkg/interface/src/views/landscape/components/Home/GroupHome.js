import React from 'react';
import { Box } from '@tlon/indigo-react';

import { EnableGroupFeed } from './EnableGroupFeed';
import { EmptyGroupHome } from './EmptyGroupHome';
import { GroupFeed } from './GroupFeed';
import { AddFeedBanner } from './AddFeedBanner';
import {Route, useHistory} from 'react-router-dom';


export function GroupHome(props) {
  const {
    associations,
    api,
    groupPath,
    groups,
    graphs,
    baseUrl,
    contacts,
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

  const graphPath = metadata?.config?.group?.resource;
  const history = useHistory();

  return (
    <Box width="100%" height="100%" overflow="hidden">
      <Route path={`${baseUrl}/enable`}
        render={() => {
          return (
            <EnableGroupFeed
              groupPath={groupPath}
              dismiss={() => history.push(baseUrl)}
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
          associations={associations}
          groups={groups}
          contacts={contacts}
          graphPath={graphPath}
          graphs={graphs}
          api={api}
          history={history}
          baseUrl={baseUrl} />
      ) : (
        <EmptyGroupHome {...props} />
      )}
    </Box>
  );
}
