import React, { ReactElement } from 'react';
import { RouteComponentProps } from 'react-router-dom';

import { Col, Box, Text, Row } from '@tlon/indigo-react';
import { Contacts, Rolodex, Groups, Associations, Graph, Association, Unreads } from '@urbit/api';

import { NotebookPosts } from './NotebookPosts';
import { useShowNickname } from '~/logic/lib/util';
import useContactState from '~/logic/state/contact';
import useGroupState from '~/logic/state/group';

interface NotebookProps {
  ship: string;
  book: string;
  graph: Graph;
  association: Association;
  baseUrl: string;
  rootUrl: string;
  unreads: Unreads;
}

export function Notebook(props: NotebookProps & RouteComponentProps): ReactElement | null {
  const {
    ship,
    book,
    association,
    graph
  } = props;

  const groups = useGroupState(state => state.groups);

  const group = groups[association?.group];
  if (!group) {
    return null; // Waiting on groups to populate
  }

  const relativePath = (p: string) => props.baseUrl + p;
  const contacts = useContactState(state => state.contacts);

  const contact = contacts?.[`~${ship}`];

  const showNickname = useShowNickname(contact);

  return (
    <Col gapY="4" pt={4} mx="auto" px={3} maxWidth="768px">
      <Row justifyContent="space-between">
        <Box>
          <Text display='block'>{association.metadata?.title}</Text>
          <Text color="lightGray">by </Text>
          <Text fontFamily={showNickname ? 'sans' : 'mono'}>
            {showNickname ? contact?.nickname : ship}
          </Text>
        </Box>
      </Row>
      <Box borderBottom="1" borderBottomColor="washedGray" />
      <NotebookPosts
        graph={graph}
        host={ship}
        book={book}
        baseUrl={props.baseUrl}
        group={group}
      />
    </Col>
  );
}

export default Notebook;
