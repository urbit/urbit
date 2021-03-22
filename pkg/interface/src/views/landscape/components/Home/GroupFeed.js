import React, {
  useEffect
} from 'react';
import { Switch, Route } from 'react-router-dom';
import { Box } from '@tlon/indigo-react'
import { resourceFromPath } from '~/logic/lib/group';
import useGraphState from '~/logic/state/graph';
import { GroupFeedHeader } from './GroupFeedHeader';
import { PostTimeline } from './Post/PostTimeline';
import { PostReplies } from './Post/PostReplies';


export function GroupFeed(props) {
  const {
    baseUrl,
    api,
    history,
    associations,
    graphPath
  } = props;
  const graphs = useGraphState(state => state.graphs);
  const pendingSize = useGraphState(state => state.pendingSize);
  const graphResource = resourceFromPath(graphPath);
  const relativePath = (path) => baseUrl + path;

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
      <Switch>
        <Route
          exact
          path={[relativePath('/'), relativePath('/feed')]}
          render={(routeProps) => {
            return (
              <PostTimeline
                {...props}
                graphs={graphs}
                pendingSize={pendingSize}
                graphResource={graphResource} />
            );
          }} />
        <Route
          path={relativePath('/feed/:index+')}
          render={(routeProps) => {
            return (
              <PostReplies
                {...props}
                graphs={graphs}
                pendingSize={pendingSize}
                graphResource={graphResource} />
            );
          }} />
      </Switch>
    </Box>
  );
}

