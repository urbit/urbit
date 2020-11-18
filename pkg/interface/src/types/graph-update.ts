import { Patp } from "./noun";
import { BigIntOrderedMap } from "~/logic/lib/BigIntOrderedMap";

export interface TextContent {
  text: string;
}
export interface UrlContent {
  url: string;
}
export interface CodeContent {
  expresssion: string;
  output: string;
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

export interface GraphNode {
  children: Graph;
  post: Post;
}

export type Graph = BigIntOrderedMap<GraphNode>;

export type Graphs = { [rid: string]: Graph };
