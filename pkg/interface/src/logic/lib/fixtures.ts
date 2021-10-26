import { Content, GraphNode, unixToDa } from '@urbit/api';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
import bigInt, { BigInteger } from 'big-integer';

export const makeComment = (
  author: string,
  time: number,
  parentIndex: string,
  contents: Content[]
): [BigInteger, GraphNode] => {
  const da = unixToDa(time);
  const index = `${parentIndex}/${da.toString()}`;

  const children = new BigIntOrderedMap<GraphNode>().gas([
    [
      bigInt.one,
      {
        post: {
          index: `${index}/1`,
          author,
          'time-sent': time,
          signatures: [],
          contents: contents,
          hash: null
        },
        children: new BigIntOrderedMap()
      }
    ]
  ]);

  return [
    da,
    {
      post: {
        index,
        author,
        'time-sent': time,
        signatures: [],
        contents: [],
        hash: null
      },
      children
    }
  ];
};
