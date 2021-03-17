import React, {
  useEffect
} from 'react';
import { Box, Row, Text } from '@tlon/indigo-react'
import { GroupFeedHeader } from './GroupFeedHeader';
import { PostInput } from './PostInput';
import { PostFeed } from './PostFeed';
import { Loading } from '~/views/components/Loading';
import { resourceFromPath } from '~/logic/lib/group';


export function GroupFeed(props) {
  const {
    baseUrl,
    api,
    history,
    graphs,
    associations,
    groups,
    contacts,
    graphPath
  } = props;
  const graphResource = resourceFromPath(graphPath);
  const graphId = `${graphResource.ship.slice(1)}/${graphResource.name}`;
  const shouldRenderFeed = graphId in graphs;

  useEffect(() => {
    api.graph.getGraph(graphResource.ship, graphResource.name);
  }, [graphPath]);

  return (
    <Box
      width="100%"
      height="100%"
      display="flex"
      flexDirection="column"
      alignItems="center"
      overflow="hidden">
      <GroupFeedHeader baseUrl={baseUrl} history={history} />
      <Box
        width="100%"
        maxWidth="616px"
        pt="4"
        pl="2"
        pr="2"
        flexDirection="column"
        alignItems="center">
        { shouldRenderFeed ? (
            <PostInput api={api} graphPath={graphPath} />
          ) : null
        }
      </Box> 
      <Box height="calc(100% - 136px)" width="100%" alignItems="center" pl="1">
        { shouldRenderFeed ? (
            <PostFeed
              graphResource={graphResource}
              graph={graphs[graphId]}
              associations={associations}
              groups={groups}
              contacts={contacts}
              api={api}
            />
          ) : <Loading />
        }
      </Box>
    </Box>
  );
}

