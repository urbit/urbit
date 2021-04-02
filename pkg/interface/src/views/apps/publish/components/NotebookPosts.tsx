import React, { Component } from 'react';
import { Col } from '@tlon/indigo-react';
import { NotePreview } from './NotePreview';
import { Contacts, Graph, Unreads, Group } from '@urbit/api';
import useContactState from '~/logic/state/contact';

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
              contact={contacts[`~${node.post.author}`]}
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
