import { Col } from '@tlon/indigo-react';
import React, {
  useEffect
} from 'react';
import { Route, Switch, useHistory } from 'react-router-dom';
import { resourceFromPath } from '~/logic/lib/group';
import useGraphState, { useGraphTimesentMap } from '~/logic/state/graph';
import { useGroup } from '~/logic/state/group';
import { useAssocForGraph } from '~/logic/state/metadata';
import { Loading } from '~/views/components/Loading';
import { GroupFeedHeader } from './GroupFeedHeader';
import { PostThreadRoutes } from './Post/PostThread';
import PostFlatTimeline from './Post/PostFlatTimeline';
import airlock from '~/logic/api';
import { markCountAsRead } from '@urbit/api';
import { PostRepliesRoutes } from './Post/PostReplies';

function GroupFlatFeed(props) {
  const {
    baseUrl,
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
  const getDeepOlderThan = useGraphState(s => s.getDeepOlderThan);

  useEffect(() => {
    //  TODO: VirtualScroller should support lower starting values than 100
    if (graphRid.ship === '~zod' && graphRid.name === 'null') {
      return;
    }
    getDeepOlderThan(graphRid.ship, graphRid.name, 100);
    airlock.poke(markCountAsRead(graphPath));
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
        <Route exact path={[relativePath('/'), relativePath('/feed')]}>
          <PostFlatTimeline
            baseUrl={relativePath('/feed')}
            graphPath={graphPath}
            group={group}
            association={association}
            vip={vip}
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

export { GroupFlatFeed };
