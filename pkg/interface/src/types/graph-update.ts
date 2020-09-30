import {Patp} from "./noun";


export interface TextContent { text: string; };
export interface UrlContent { url: string; }
export interface CodeContent { expresssion: string; output: string; };
export interface ReferenceContent { uid: string; }
export type Content =  TextContent | UrlContent | CodeContent | ReferenceContent;

export interface Post {
  author: Patp;
  contents: Content[];
  hash?: string;
  index: string;
  pending?: boolean;
  signatures: string[];
  'time-sent': number;
}


export interface GraphNode {
  children: Graph;
  post: Post;
}

export type Graph = Map<number, GraphNode>;

export type Graphs = { [rid: string]: Graph };


