import React from 'react';
import { Link, RouteComponentProps } from 'react-router-dom';
import { NotebookPosts } from './NotebookPosts';
import { Box, Button, Text, Row, Col } from '@tlon/indigo-react';
import { Groups } from '~/types/group-update';
import { Contacts, Rolodex } from '~/types/contact-update';
import GlobalApi from '~/logic/api/global';
import { Associations, Graph, Association } from '~/types';

interface NotebookProps {
  api: GlobalApi;
  ship: string;
  book: string;
  graph: Graph;
  notebookContacts: Contacts;
  association: Association;
  associations: Associations;
  contacts: Rolodex;
  groups: Groups;
  hideNicknames: boolean;
  hideAvatars: boolean;
  baseUrl: string;
  rootUrl: string;
}

interface NotebookState {
  isUnsubscribing: boolean;
  tab: string;
}

export function Notebook(props: NotebookProps & RouteComponentProps) {
  const {
    ship,
    book,
    notebookContacts,
    groups,
    hideNicknames,
    hideAvatars,
    association,
    graph
  } = props;
  const { metadata } = association;

  const group = groups[association?.['group-path']];
  if (!group) {
    return null; // Waitin on groups to populate
  }

  const relativePath = (p: string) => props.baseUrl + p;

  const contact = notebookContacts?.[ship];
  const isOwn = `~${window.ship}` === ship;
  let isWriter = true;

  if (group.tags?.publish?.[`writers-${book}`]) {
    isWriter = isOwn || group.tags?.publish?.[`writers-${book}`]?.has(window.ship);
  }

  const showNickname = contact?.nickname && !hideNicknames;

  return (
    <Col gapY="4" pt={4} mx="auto" px={3} maxWidth="768px">
      <Row justifyContent="space-between">
        <Box>
          <Text display='block'>{metadata?.title}</Text>
          <Text color="lightGray">by </Text>
          <Text fontFamily={showNickname ? 'sans' : 'mono'}>
            {showNickname ? contact?.nickname : ship}
          </Text>
        </Box>
        {isWriter && (
          <Link to={relativePath('/new')}>
            <Button primary style={{ cursor: 'pointer' }}>
              New Post
            </Button>
          </Link>
        )}
      </Row>
      <Box borderBottom="1" borderBottomColor="washedGray" />
      <NotebookPosts
        graph={graph}
        host={ship}
        book={book}
        contacts={notebookContacts ? notebookContacts : {}}
        hideNicknames={hideNicknames}
        hideAvatars={hideAvatars}
        baseUrl={props.baseUrl}
        api={props.api}
        group={group}
      />
    </Col>
  );
}

export default Notebook;
