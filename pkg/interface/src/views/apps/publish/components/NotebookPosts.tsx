import React from 'react';
import { Col } from '@tlon/indigo-react';
import { NotePreview } from './NotePreview';
import { Contacts, Graph, Group } from '~/types';
import GlobalApi from '~/logic/api/global';

interface NotebookPostsProps {
  contacts: Contacts;
  graph: Graph;
  host: string;
  book: string;
  baseUrl: string;
  hideAvatars?: boolean;
  hideNicknames?: boolean;
  api: GlobalApi;
  group: Group;
}

export function NotebookPosts(props: NotebookPostsProps) {
  return (
    <Col>
      {Array.from(props.graph || []).map(
        ([date, node]) =>
          node && (
            <NotePreview
              key={date.toString()}
              host={props.host}
              book={props.book}
              contacts={props.contacts}
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
