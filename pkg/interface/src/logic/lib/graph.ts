import { Graph } from '@urbit/api';
import { BigInteger } from 'big-integer';
import _ from 'lodash';
import useMetadataState from '~/logic/state/metadata';

export function getNodeFromGraph(graph: Graph, index: BigInteger[]) {
  return _.reduce(
    index.slice(1),
    (acc, val) => {
      return acc?.children?.get(val);
    },
    graph.get(index[0])
  );
}

export function getPostRoute(
  graph: string,
  index: BigInteger[],
  thread = false
) {
  const association = useMetadataState.getState().associations.graph[graph];
  const segment = thread ? 'thread' : 'replies';

  return `/~landscape${association.group}/feed/${segment}/${index
    .map(i => i.toString())
    .join('/')}`;
}
