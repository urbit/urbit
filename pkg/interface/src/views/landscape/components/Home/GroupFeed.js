import React, {
  useEffect
} from 'react';
import { Switch, Route } from 'react-router-dom';
import { Col } from '@tlon/indigo-react'
import { resourceFromPath } from '~/logic/lib/group';
import useGraphState from '~/logic/state/graph';
import { GroupFeedHeader } from './GroupFeedHeader';
import { useHistory } from 'react-router-dom';

import PostTimeline from './Post/PostTimeline';
import PostReplies from './Post/PostReplies';

import useMetadataState from '~/logic/state/metadata';
import useGroupState from '~/logic/state/group';


function GroupFeed(props) {
  const {
    baseUrl,
    api,
    graphPath,
    groupPath,
    vip
  } = props;

  const groups = useGroupState(state => state.groups);
  const group = groups[groupPath];

  const associations = useMetadataState(state => state.associations);
  const graphs = useGraphState(state => state.graphs);
  const graphResource = resourceFromPath(graphPath);
  const graphTimesentMap = useGraphState(state => state.graphTimesentMap);

  const pendingSize = Object.keys(
    graphTimesentMap[`${graphResource.ship.slice(1)}/${graphResource.name}`] ||
    {}
  ).length;

  const relativePath = (path) => baseUrl + path;
  const association = associations.graph[graphPath];

  const history = useHistory();
  const locationUrl = history.location.pathname;

  const graphId = `${graphResource.ship.slice(1)}/${graphResource.name}`;
  const graph = graphs[graphId];

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
        vip={vip}
        graphResource={graphResource} />
      <Switch>
        <Route
          exact
          path={[relativePath('/'), relativePath('/feed')]}
          render={(routeProps) => {
            return (
              <PostTimeline
                baseUrl={baseUrl}
                api={api}
                history={history}
                graphPath={graphPath}
                group={group}
                association={association}
                vip={vip}
                graph={graph}
                pendingSize={pendingSize} />
            );
          }} />
        <Route
          path={relativePath('/feed/:index+')}
          render={(routeProps) => {
            return (
              <PostReplies
                locationUrl={locationUrl}
                baseUrl={baseUrl}
                api={api}
                history={history}
                graphPath={graphPath}
                group={group}
                association={association}
                vip={vip}
                graph={graph}
                pendingSize={pendingSize} />
            );
          }} />
      </Switch>
    </Col>
  );
}

export { GroupFeed };
