import { Post, GraphNode, TextContent, Graph, NodeMap } from '@urbit/api';
import { buntPost } from '~/logic/lib/post';
import { unixToDa } from '~/logic/lib/util';
import { BigIntOrderedMap } from './BigIntOrderedMap';
import bigInt, { BigInteger } from 'big-integer';
import tokenizeMessage from './tokenizeMessage';

export function newPost(
  title: string,
  body: string
): [BigInteger, NodeMap] {
  const now = Date.now();
  const nowDa = unixToDa(now);
  const root: Post = {
    author: `~${window.ship}`,
    index: '/' + nowDa.toString(),
    'time-sent': now,
    contents: [],
    hash: null,
    signatures: []
  };

  // re-enable on mainnet deploy
  //const tokenisedBody = tokenizeMessage(body);

  const revContainer: Post = { ...root, index: root.index + '/1' };
  const commentsContainer = { ...root, index: root.index + '/2' };

  const firstRevision: Post = {
    ...revContainer,
    index: revContainer.index + '/1',
    contents: [{ text: title }, { text: body }]
    //contents: [{ text: title }, { text: body } ...tokenisedBody]
  };

  const nodes = {
    [root.index]: {
      post: root,
      children: {
          1: {
            post: revContainer,
            children: {
              1: {
                post: firstRevision,
                children: null
              }
            }
          },
          2: {
            post: commentsContainer,
            children: null
          }
      }
    }
  };

  return [nowDa, nodes];
}

export function editPost(rev: number, noteId: BigInteger, title: string, body: string) {
  const now = Date.now();
  // reenable
  //const tokenisedBody = tokenizeMessage(body);
  const newRev: Post = {
    author: `~${window.ship}`,
    index: `/${noteId.toString()}/1/${rev}`,
    'time-sent': now,
    //contents: [{ text: title }, ...tokenisedBody],
    contents: [{ text: title }, { text: body }],
    hash: null,
    signatures: []
  };
  const nodes = {
    [newRev.index]: {
      post: newRev,
      children: null
    }
  };

  return nodes;
}

export function getLatestRevision(node: GraphNode): [number, string, any, Post] {
  const revs = node.children.get(bigInt(1));
  const empty = [1, '', '', buntPost()] as [number, string, string, Post];
  if(!revs) {
    return empty;
  }
  const [revNum, rev] = [...revs.children][0];
  if(!rev) {
    return empty;
  }
  const title = rev.post.contents[0];
  const body = rev.post.contents.slice(1);
  return [revNum.toJSNumber(), title.text, body, rev.post];
}

export function getLatestCommentRevision(node: GraphNode): [number, Post] {
  const empty = [1, buntPost()] as [number, Post];
  if (node.children.size <= 0) {
    return empty;
  }
  const [revNum, rev] = [...node.children][0];
  if(!rev) {
    return empty;
  }
  return [revNum.toJSNumber(), rev.post];
}

export function getComments(node: GraphNode): GraphNode {
  const comments = node.children.get(bigInt(2));
  if(!comments) {
    return { post: buntPost(), children: new BigIntOrderedMap() };
  }
  return comments;
}

export function getSnippet(body: any) {
  const firstContent = Object.values(body[0])[0];
  const newlineIdx = firstContent.indexOf('\n', 2);
  const end = newlineIdx > -1 ? newlineIdx : firstContent.length;
  const start = firstContent.substr(0, end);

  return (start === firstContent || firstContent.startsWith('![')) ? start : `${start}...`;
}
