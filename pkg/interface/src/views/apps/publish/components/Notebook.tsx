import React from "react";
import { RouteComponentProps } from "react-router-dom";
import { NotebookPosts } from "./NotebookPosts";
import { Col } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Contacts, Rolodex, Groups, Associations, Graph, Association, Unreads } from "~/types";

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

  return (
    <Col gapY="4" pt={4} mx="auto" px={3} maxWidth="768px">
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
