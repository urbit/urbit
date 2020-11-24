import React, { Component } from "react";
import { Col } from "@tlon/indigo-react";
import { NotePreview } from "./NotePreview";
import { Contacts, Graph, Unreads } from "~/types";

interface NotebookPostsProps {
  contacts: Contacts;
  graph: Graph;
  host: string;
  book: string;
  hideNicknames?: boolean;
  baseUrl: string;
  unreads: Unreads;
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
              unreads={props.unreads}
              contact={props.contacts[node.post.author]}
              node={node}
              hideNicknames={props.hideNicknames}
              baseUrl={props.baseUrl}
            />
          )
      )}
    </Col>
  );
}

export default NotebookPosts;
