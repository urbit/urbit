import { Content, GraphNode, Post } from '@urbit/api';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
import bigInt, { BigInteger } from 'big-integer';
import { buntPost } from '~/logic/lib/post';
import { unixToDa } from '~/logic/lib/util';
import tokenizeMessage from './tokenizeMessage';

export function newPost(
  title: string,
  body: string
): [BigInteger, any] {
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

  const tokenisedBody = tokenizeMessage(body);

  const revContainer: Post = { ...root, index: root.index + '/1' };
  const commentsContainer = { ...root, index: root.index + '/2' };

  const firstRevision: Post = {
    ...revContainer,
    index: revContainer.index + '/1',
    contents: [{ text: title }, ...tokenisedBody]
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
  const tokenisedBody = tokenizeMessage(body);
  const newRev: Post = {
    author: `~${window.ship}`,
    index: `/${noteId.toString()}/1/${rev}`,
    'time-sent': now,
    contents: [{ text: title }, ...tokenisedBody],
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

export function getLatestRevision(node: GraphNode): [number, string, string, Post] {
  const empty = [1, '', '', buntPost()] as [number, string, string, Post];
  const revs = node.children?.get(bigInt(1));
  if(!revs) {
    return empty;
  }
  let revNum, rev;
  if (revs?.children !== null) {
    [revNum, rev] = [...revs.children][0];
  }
  if (!rev) {
    return empty;
  }
  const title = rev.post.contents[0];
  const body = rev.post.contents.slice(1);
  return [revNum.toJSNumber(), title.text, body, rev.post];
}

export function getLatestCommentRevision(node: GraphNode): [number, Post] {
  const empty = [1, buntPost()] as [number, Post];
  const childSize = node?.children?.size ?? 0;
  if (childSize <= 0) {
    return empty;
  }
  let revNum, rev;
  if (node?.children !== null) {
    [revNum, rev] = [...node.children][0];
  }
  if (!rev) {
    return empty;
  }
  return [revNum.toJSNumber(), rev.post];
}

export function getComments(node: GraphNode): GraphNode {
  const comments = node.children?.get(bigInt(2));
  if(!comments) {
    return { post: buntPost(), children: new BigIntOrderedMap() };
  }
  return comments;
}

export function getSnippet(body: string | Content[]) {
  const firstContent = Object.values(body[0])[0];
  const newlineIdx = firstContent.indexOf('\n', 2);
  const end = newlineIdx > -1 ? newlineIdx : firstContent.length;
  const start = firstContent.substr(0, end);

  return (start === firstContent || firstContent.startsWith('![')) ? start : `${start}...`;
}
