import React from 'react';
import { Box } from '@tlon/indigo-react'
import { PostInput } from './PostInput';
import { PostFeed } from './PostFeed';
import { Loading } from '~/views/components/Loading';


export function PostTimeline(props) {
  const {
    baseUrl,
    api,
    history,
    associations,
    groups,
    contacts,
    graphPath,
    graphs,
    pendingSize,
    graphResource
  } = props;
  const graphId = `${graphResource.ship.slice(1)}/${graphResource.name}`;
  const shouldRenderFeed = graphId in graphs;

  return (
    <>
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
      <Box height="calc(100% - 176px)" width="100%" alignItems="center" pl="1">
        { shouldRenderFeed ? (
            <PostFeed
              graphResource={graphResource}
              graph={graphs[graphId]}
              pendingSize={pendingSize}
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
    </>
  );
}

