import { Box, Button, Col, Row, Text } from '@tlon/indigo-react';
import { Association, Graph } from '@urbit/api';
import React, { ReactElement, useCallback } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
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
  api: GlobalApi;
}

export function Notebook(props: NotebookProps & RouteComponentProps): ReactElement | null {
  const {
    ship,
    book,
    association,
    graph,
    api
  } = props;

  const groups = useGroupState(state => state.groups);
  const contacts = useContactState(state => state.contacts);

  const group = groups[association?.group];

  const contact = contacts?.[`~${ship}`];

  const showNickname = useShowNickname(contact);

  const readBook = useCallback(() => {
    api.hark.readGraph(association.resource);
  }, [association.resource]);

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
        <Button onClick={readBook}>Mark all as Read</Button>
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
