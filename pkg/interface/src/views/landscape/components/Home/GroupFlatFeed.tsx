import { Col } from '@tlon/indigo-react';
import React, {
  useEffect
} from 'react';
import { Route, Switch, useHistory } from 'react-router-dom';
import { resourceFromPath } from '~/logic/lib/group';
import { useFlatGraph, useGraphTimesentMap } from '~/logic/state/graph';
import { useGroup } from '~/logic/state/group';
import { useAssocForGraph } from '~/logic/state/metadata';
import { Loading } from '~/views/components/Loading';
import { GroupFeedHeader } from './GroupFeedHeader';
import PostThread from './Post/PostThread';
import PostFlatTimeline from './Post/PostFlatTimeline';
import PostReplies from './Post/PostReplies';


function GroupFlatFeed(props) {
  const {
    baseUrl,
    api,
    graphPath,
    groupPath,
    vip
  } = props;

  const group = useGroup(groupPath);

  const graphRid =
    graphPath ? resourceFromPath(graphPath) : resourceFromPath('/ship/~zod/null');

  const association = useAssocForGraph(graphPath);
  
  const graphTimesentMap = useGraphTimesentMap(graphRid.ship, graphRid.name);
  const pendingSize = Object.keys(graphTimesentMap || {}).length;

  const relativePath = path => baseUrl + path;
  const history = useHistory();
  const locationUrl = history.location.pathname;

  useEffect(() => {
    //  TODO: VirtualScroller should support lower starting values than 100
    if (graphRid.ship === '~zod' && graphRid.name === 'null') {
      return;
    }
    api.graph.getDeepOlderThan(graphRid.ship, graphRid.name, null, 100);
    api.hark.markCountAsRead(association, '/', 'post');
  }, [graphPath]);

  if (!graphPath) {
    return <Loading />;
  }

  return (
    <Col
      width="100%"
      height="100%"
      display="flex"
      overflow="hidden"
      position="relative"
      alignItems="center"
    >
      <GroupFeedHeader
        baseUrl={baseUrl}
        history={history}
        vip={vip}
        graphResource={graphRid}
      />
      <Switch>
        <Route
          exact
          path={[relativePath('/'), relativePath('/feed')]}
          render={(routeProps) => {
            return (
              <PostFlatTimeline
                baseUrl={baseUrl}
                api={api}
                history={history}
                graphPath={graphPath}
                group={group}
                association={association}
                vip={vip}
                pendingSize={pendingSize}
              />
            );
          }}
        />
        <Route
          path={relativePath('/feed/thread/:index+')}
          render={(routeProps) => {
            return (
              <PostThread
                locationUrl={locationUrl}
                baseUrl={baseUrl}
                api={api}
                history={history}
                graphPath={graphPath}
                group={group}
                association={association}
                vip={vip}
                pendingSize={pendingSize}
              />
            );
          }}
        />
        <Route
          path={relativePath('/feed/replies/:index+')}
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
                pendingSize={pendingSize}
              />
            );
          }}
        />
      </Switch>
    </Col>
  );
}

export { GroupFlatFeed };
