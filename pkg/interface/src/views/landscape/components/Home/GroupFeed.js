import React, {
  useEffect
} from 'react';
import { Box, Row, Text } from '@tlon/indigo-react'
import { GroupFeedHeader } from './GroupFeedHeader';
import { PostInput } from './Post/PostInput';
import { PostFeed } from './Post/PostFeed';
import { Loading } from '~/views/components/Loading';
import { resourceFromPath } from '~/logic/lib/group';
import useGraphState from '~/logic/state/graph';


export function GroupFeed(props) {
  const {
    baseUrl,
    api,
    history,
    associations,
    groups,
    contacts,
    graphPath
  } = props;
  const graphResource = resourceFromPath(graphPath);
  const graphId = `${graphResource.ship.slice(1)}/${graphResource.name}`;
  const graphs = useGraphState(state => state.graphs);

  const shouldRenderFeed = graphId in graphs;

  useEffect(() => {
    //  TODO: VirtualScroller should support lower starting values than 100
    api.graph.getNewest(graphResource.ship, graphResource.name, 100);
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
        pt="3"
        pl="2"
        pr="2"
        mb="3"
        flexDirection="column"
        alignItems="center">
        { shouldRenderFeed ? (
            <PostInput api={api} graphResource={graphResource} />
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
              history={history}
              baseUrl={baseUrl}
            />
          ) : <Loading />
        }
      </Box>
    </Box>
  );
}

