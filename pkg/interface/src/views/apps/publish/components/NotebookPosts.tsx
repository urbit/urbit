import React, { Component } from "react";
import { Col } from "@tlon/indigo-react";
import { NotePreview } from "./NotePreview";
import { Contacts, Graph } from "~/types";

interface NotebookPostsProps {
  contacts: Contacts;
  graph: Graph;
  host: string;
  book: string;
  baseUrl: string;
  hideNicknames?: boolean;
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
            />
          )
      )}
    </Col>
  );
}

export default NotebookPosts;
