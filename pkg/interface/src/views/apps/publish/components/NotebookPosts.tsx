import React from 'react';

import { Col } from '@tlon/indigo-react';
import { Graph, Unreads, Group } from '@urbit/api';

import { NotePreview } from './NotePreview';
import useContactState from '~/logic/state/contacts';

interface NotebookPostsProps {
  graph: Graph;
  host: string;
  book: string;
  baseUrl: string;
  unreads: Unreads;
  hideAvatars?: boolean;
  hideNicknames?: boolean;
  group: Group;
}

export function NotebookPosts(props: NotebookPostsProps) {
  const contacts = useContactState(state => state.contacts);
  return (
    <Col>
      {Array.from(props.graph || []).map(
        ([date, node]) =>
          node && (
            <NotePreview
              key={date.toString()}
              host={props.host}
              book={props.book}
              unreads={props.unreads}
              contact={contacts[node.post.author]}
              node={node}
              baseUrl={props.baseUrl}
              api={props.api}
              group={props.group}
            />
          )
      )}
    </Col>
  );
}

export default NotebookPosts;
