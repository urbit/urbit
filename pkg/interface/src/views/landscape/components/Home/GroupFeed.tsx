import { Col } from '@tlon/indigo-react';
import { deSig, markCountAsRead } from '@urbit/api';
import React, {
  useEffect
} from 'react';
import { Route, Switch, useHistory } from 'react-router-dom';
import { resourceFromPath } from '~/logic/lib/group';
import useGraphState from '~/logic/state/graph';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';
import { Loading } from '~/views/components/Loading';
import { GroupFeedHeader } from './GroupFeedHeader';
import { PostRepliesRoutes } from './Post/PostReplies';
import PostTimeline from './Post/PostTimeline';
import airlock from '~/logic/api';
import { PostThreadRoutes } from './Post/PostThread';
import { toHarkPlace } from '~/logic/lib/util';

function GroupFeed(props) {
  const {
    baseUrl,
    graphPath,
    groupPath,
    vip
  } = props;

  const groups = useGroupState(state => state.groups);
  const group = groups[groupPath];

  const associations = useMetadataState(state => state.associations);
  const graphs = useGraphState(state => state.graphs);
  const getNewest = useGraphState(s => s.getNewest);
  const graphResource =
    graphPath ? resourceFromPath(graphPath) : resourceFromPath('/ship/~zod/null');
  const graphTimesentMap = useGraphState(state => state.graphTimesentMap);

  const pendingSize = Object.keys(
    graphTimesentMap[`${deSig(graphResource.ship)}/${graphResource.name}`] ||
    {}
  ).length;

  const relativePath = path => baseUrl + path;
  const association = associations.graph[graphPath];

  const history = useHistory();

  const graphId = `${deSig(graphResource.ship)}/${graphResource.name}`;
  const graph = graphs[graphId];

  useEffect(() => {
    //  TODO: VirtualScroller should support lower starting values than 100
    if (graphResource.ship === '~zod' && graphResource.name === 'null') {
      return;
    }
    getNewest(graphResource.ship, graphResource.name, 100);

    airlock.poke(markCountAsRead(toHarkPlace(graphPath)));
  }, [graphPath]);

  if (!graphPath) {
    return <Loading />;
  }

  return (
    <Col
      width="100%"
      height="100%"
      display="flex"
      position="relative"
      alignItems="center"
    >
      <GroupFeedHeader
        baseUrl={baseUrl}
        history={history}
        graphs={graphs}
        vip={vip}
        graphResource={graphResource}
      />
      <Switch>
        <Route exact path={relativePath('/feed')}>
          <PostTimeline
            baseUrl={relativePath('/feed')}
            history={history}
            graphPath={graphPath}
            group={group}
            association={association}
            vip={vip}
            graph={graph}
            pendingSize={pendingSize}
          />
        </Route>
        <Route path={relativePath('/feed/thread')}>
          <PostThreadRoutes
            baseUrl={relativePath('/feed/thread')}
            association={association}
            vip={vip}
            pendingSize={pendingSize}
          />
        </Route>
        <Route path={relativePath('/feed/replies')}>
          <PostRepliesRoutes
            baseUrl={relativePath('/feed/replies')}
            association={association}
            vip={vip}
            pendingSize={pendingSize}
          />
        </Route>
      </Switch>
    </Col>
  );
}

export { GroupFeed };
