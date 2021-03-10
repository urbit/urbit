import { Patp } from "..";
import BigIntOrderedMap from "../lib/BigIntOrderedMap";

export interface TextContent {
  text: string;
}
export interface UrlContent {
  url: string;
}
export interface CodeContent {
  code: {
    expression: string;
    output: string[] | undefined;
  }
}

export interface ReferenceContent {
  uid: string;
}
export interface MentionContent {
  mention: string;
}
export type Content =
  | TextContent
  | UrlContent
  | CodeContent
  | ReferenceContent
  | MentionContent;

export interface Post {
  author: Patp;
  contents: Content[];
  hash: string | null;
  index: string;
  pending?: boolean;
  signatures: string[];
  "time-sent": number;
}

export interface GraphNodePoke {
  post: Post;
  children: GraphChildrenPoke | null;
}

export interface GraphChildrenPoke {
  [k: string]: GraphNodePoke;
}

export interface GraphNode {
  children: Graph | null;
  post: Post;
}

export type Graph = BigIntOrderedMap<GraphNode>;

export type Graphs = { [rid: string]: Graph };
