import React, { Component } from "react";
import { Col } from "@tlon/indigo-react";
import { NotePreview } from "./NotePreview";
import { Contacts, Graph } from "~/types";

interface NotebookPostsProps {
  contacts: Contacts;
  graph: Graph;
  host: string;
  book: string;
  hideNicknames?: boolean;
  baseUrl: string;
}

export function NotebookPosts(props: NotebookPostsProps) {
  return (
    <Col>
      {Array.from(props.graph || []).map(
        ([date, node]) =>
          node && (
            <NotePreview
              key={date}
              host={props.host}
              book={props.book}
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
