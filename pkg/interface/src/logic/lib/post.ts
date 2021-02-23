import { Post, GraphNode } from '@urbit/api';

export const buntPost = (): Post => ({
  author: '',
  contents: [],
  hash: null,
  index: '',
  signatures: [],
  'time-sent': 0
});

export function makeNodeMap(posts: Post[]): Record<string, GraphNode> {
  const nodes = {};
  posts.forEach((p) => {
    nodes[p.index] = { children: { empty: null }, post: p };
  });
  return nodes;
}
