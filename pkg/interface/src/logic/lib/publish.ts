import { Post, GraphNode, TextContent, Graph, NodeMap } from "~/types";
import { buntPost } from '~/logic/lib/post';
import { unixToDa } from "~/logic/lib/util";
import {BigIntOrderedMap} from "./BigIntOrderedMap";
import bigInt, {BigInteger} from 'big-integer';

export function newPost(
  title: string,
  body: string
): [BigInteger, NodeMap] {
  const now = Date.now();
  const nowDa = unixToDa(now);
  const root: Post = {
    author: `~${window.ship}`,
    index: "/" + nowDa.toString(),
    "time-sent": now,
    contents: [],
    hash: null,
    signatures: [],
  };

  const revContainer: Post = { ...root, index: root.index + "/1" };
  const commentsContainer = { ...root, index: root.index + "/2" };

  const firstRevision: Post = {
    ...revContainer,
    index: revContainer.index + "/1",
    contents: [{ text: title }, { text: body }],
  };

  const nodes = {
    [root.index]: {
      post: root,
      children: {
        graph: {
          1: {
            post: revContainer,
            children: {
              graph: {
                1: {
                  post: firstRevision,
                  children: { empty: null },
                },
              },
            },
          },
          2: {
            post: commentsContainer,
            children: { empty: null },
          },
        },
      },
    },
  };

  return [nowDa, nodes];
}

export function editPost(rev: number, noteId: BigInteger, title: string, body: string) {
  const now = Date.now();
  const newRev: Post = {
    author: `~${window.ship}`,
    index: `/${noteId.toString()}/1/${rev}`,
    "time-sent": now,
    contents: [{ text: title }, { text: body }],
    hash: null,
    signatures: [],
  };
  const nodes = {
    [newRev.index]: {
      post: newRev,
      children: { empty: null }
    }
  };

  return nodes;
}

export function getLatestRevision(node: GraphNode): [number, string, string, Post] {
  const revs = node.children.get(bigInt(1));
  const empty = [1, "", "", buntPost()] as [number, string, string, Post];
  if(!revs) {
    return empty;
  }
  const [revNum, rev] = [...revs.children][0];
  if(!rev) {
    return empty
  }
  const [title, body] = rev.post.contents as TextContent[];
  return [revNum.toJSNumber(), title.text, body.text, rev.post];
}

export function getComments(node: GraphNode): GraphNode {
  const comments = node.children.get(bigInt(2));
  if(!comments) {
    return { post: buntPost(), children: new BigIntOrderedMap() } 
  }
  return comments;
}

export function getSnippet(body: string) {
  const start = body.slice(0, 400);
  return start === body ? start : `${start}...`;
}
 
