import { Box, Col, Text } from '@tlon/indigo-react';
import bigInt, { BigInteger } from 'big-integer';
import React, {
  useCallback,
  useEffect
} from 'react';
import { resourceFromPath } from '~/logic/lib/group';
import { Loading } from '~/views/components/Loading';
import { arrToString } from '@urbit/api/lib/BigIntArrayOrderedMap';
import useGraphState from '~/logic/state/graph';
import PostFlatFeed from './PostFlatFeed';
import PostInput from './PostInput';
import { Association, PermVariation } from '@urbit/api';
import { useParams, Switch, Route } from 'react-router';
import { useGroupForAssoc } from '~/logic/state/group';

export function PostThreadRoutes(props: PostThreadProps) {
  const { atom } = useParams<{ atom?: string }>();
  const index = atom
    ? [...(props.index || []), bigInt(atom)]
    : (props.index || []);

  const { baseUrl } = props;
  const makePath = (s: string) => baseUrl + s;

  return (
    <Switch>
      <Route path={makePath('/:atom')} render={(routeProps) => {
        const { url } = routeProps.match;
        return (
          <PostThreadRoutes {...props} baseUrl={url} index={index} />
          );
      }}
      />
      <Route path={baseUrl}>
        <PostThread {...props} index={index} />
      </Route>
    </Switch>
  );
}
interface PostThreadProps {
  baseUrl: string;
  association: Association;
  pendingSize: number;
  vip: PermVariation;
  index?: BigInteger[];
}

export default function PostThread(props: PostThreadProps) {
  const {
    baseUrl,
    association,
    vip,
    pendingSize,
    index = []
  } = props;

  const getFirstborn = useGraphState(s => s.getFirstborn);
  const group = useGroupForAssoc(association);

  const { ship, name } = resourceFromPath(association.resource);

  useEffect(() => {
    if (index.length < 1) {
      return;
    }

    getFirstborn(ship, name, `/${index.map(i => i.toString()).join('/')}`);
  }, [association.resource, index]);

  const graphId = `${ship.slice(1)}/${name}`;
  const threadGraph = useGraphState(useCallback(s => s.threadGraphs[graphId] || {}, [graphId]));

  const shouldRenderFeed = arrToString(index) in threadGraph;

  if (!shouldRenderFeed) {
    return (
      <Box height="100%" pt={3} pb={3} width="100%" alignItems="center" pl={1}>
        <Loading />
      </Box>
    );
  }

  //  TODO: improve performance characteristics of the useGraphState required
  //  to fetch this
  const thread = threadGraph[arrToString(index)];

  const first = thread.peekLargest()?.[0];
  if (!first) {
    return (
      <Col
        key={0}
        width="100%"
        height="100%"
        alignItems="center"
      >
        <Col
          width="100%"
          maxWidth="608px"
          pt={3}
          pl={1}
          pr={1}
          mb={3}
          alignItems="center"
        >
          <PostInput
            graphPath={association.resource}
            group={group}
            association={association}
            vip={vip}
          />
        </Col>
        <Box
          pl={1}
          pr={1}
          width="100%"
          maxWidth="608px"
          alignItems="center"
        >
          <Col bg="washedGray" width="100%" alignItems="center" p={3}>
            <Text textAlign="center" width="100%">
              No one has posted anything here yet.
            </Text>
          </Col>
        </Box>
      </Col>
    );
  }

  return (
    <Box height="calc(100% - 48px)" width="100%" alignItems="center" pl={1}>
      <PostFlatFeed
        key={location.pathname}
        graphPath={association.resource}
        flatGraph={thread}
        pendingSize={pendingSize}
        association={association}
        group={group}
        vip={vip}
        baseUrl={baseUrl}
        isThread={true}
      />
    </Box>
  );
}

