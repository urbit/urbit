import { Box, Col, Row, Text } from '@tlon/indigo-react';
import { Association, Graph } from '@urbit/api';
import React, { ReactElement } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import { useShowNickname } from '~/logic/lib/util';
import useContactState from '~/logic/state/contact';
import useGroupState from '~/logic/state/group';
import { NotebookPosts } from './NotebookPosts';

interface NotebookProps {
  ship: string;
  book: string;
  graph: Graph;
  association: Association;
  baseUrl: string;
  rootUrl: string;
}

export function Notebook(props: NotebookProps & RouteComponentProps): ReactElement | null {
  const {
    ship,
    book,
    association,
    graph
  } = props;

  const groups = useGroupState(state => state.groups);
  const contacts = useContactState(state => state.contacts);

  const group = groups[association?.group];
  const relativePath = (p: string) => props.baseUrl + p;

  const contact = contacts?.[`~${ship}`];

  const showNickname = useShowNickname(contact);

  if (!group) {
    return null; // Waiting on groups to populate
  }

  return (
    <Col gapY={4} pt={4} mx="auto" px={3} maxWidth="768px">
      <Row justifyContent="space-between">
        <Box>
          <Text display='block'>{association.metadata?.title}</Text>
          <Text color="lightGray">by </Text>
          <Text fontFamily={showNickname ? 'sans' : 'mono'}>
            {showNickname ? contact?.nickname : ship}
          </Text>
        </Box>
      </Row>
      <Box borderBottom={1} borderBottomColor="lightGray" />
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
