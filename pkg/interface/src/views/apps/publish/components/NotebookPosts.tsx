import { Col } from '@tlon/indigo-react';
import { Graph, Group } from '@urbit/api';
import React from 'react';
import { NotePreview } from './NotePreview';

interface NotebookPostsProps {
  graph: Graph;
  host: string;
  book: string;
  baseUrl: string;
  hideAvatars?: boolean;
  hideNicknames?: boolean;
  group: Group;
}

export function NotebookPosts(props: NotebookPostsProps) {
  return (
    <Col>
      {Array.from(props.graph || []).map(
        ([date, node]) =>
          node && typeof node?.post !== 'string' && (
            <NotePreview
              key={date.toString()}
              host={props.host}
              book={props.book}
              node={node}
              baseUrl={props.baseUrl}
              group={props.group}
            />
          )
      )}
    </Col>
  );
}

export default NotebookPosts;
