import React from "react";
import { RouteComponentProps, Link } from "react-router-dom";
import { NotebookPosts } from "./NotebookPosts";
import { Col, Box, Text, Button, Row } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Contacts, Rolodex, Groups, Associations, Graph, Association, Unreads } from "~/types";
import { useShowNickname } from "~/logic/lib/util";

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
  baseUrl: string;
  rootUrl: string;
  unreads: Unreads;
}

export function Notebook(props: NotebookProps & RouteComponentProps) {
  const {
    ship,
    book,
    notebookContacts,
    groups,
    association,
    graph
  } = props;

  const group = groups[association?.group];
  if (!group) {
    return null; // Waiting on groups to populate
  }


  const relativePath = (p: string) => props.baseUrl + p;

  const contact = notebookContacts?.[ship];
  const isOwn = `~${window.ship}` === ship;
  console.log(association.resource);

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
        contacts={notebookContacts ? notebookContacts : {}}
        unreads={props.unreads}
        baseUrl={props.baseUrl}
        api={props.api}
        group={group}
      />
    </Col>
  );
}

export default Notebook;
