import React, {
  useEffect
} from 'react';
import { Switch, Route } from 'react-router-dom';
import { Col } from '@tlon/indigo-react'
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
  const graphResource = resourceFromPath(graphPath);
  const graphTimesentMap = useGraphState(state => state.graphTimesentMap);
  const pendingSize = Object.keys(
    graphTimesentMap[`${graphResource.ship.slice(1)}/${graphResource.name}`] ||
    {}
  ).length;

  const relativePath = (path) => baseUrl + path;
  const association = associations.graph[graphPath];

  useEffect(() => {
    //  TODO: VirtualScroller should support lower starting values than 100
    api.graph.getNewest(graphResource.ship, graphResource.name, 100);
  }, [graphPath]);

  return (
    <Col
      width="100%"
      height="100%"
      display="flex"
      position="relative"
      alignItems="center">
      <GroupFeedHeader
        baseUrl={baseUrl}
        history={history}
        graphs={graphs}
        graphResource={graphResource} />
      <Switch>
        <Route
          exact
          path={[relativePath('/'), relativePath('/feed')]}
          render={(routeProps) => {
            return (
              <PostTimeline
                {...props}
                association={association}
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
                association={association}
                graphs={graphs}
                pendingSize={pendingSize}
                graphResource={graphResource} />
            );
          }} />
      </Switch>
    </Col>
  );
}

