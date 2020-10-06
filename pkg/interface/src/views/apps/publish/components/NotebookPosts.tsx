import React, { Component } from "react";
import { Col } from "@tlon/indigo-react";
import { Notes, NoteId } from "../../../../types/publish-update";
import { NotePreview } from "./NotePreview";
import { Contacts } from "../../../../types/contact-update";

interface NotebookPostsProps {
  list: NoteId[];
  contacts: Contacts;
  notes: Notes;
  host: string;
  book: string;
  baseUrl: string;
  hideNicknames?: boolean;
}

export function NotebookPosts(props: NotebookPostsProps) {
  return (
    <Col mt="3">
      {props.list.map((noteId: NoteId) => {
        const note = props.notes[noteId];
        if (!note) {
          return null;
        }
        return (
          <NotePreview
            key={noteId}
            host={props.host}
            book={props.book}
            note={note}
            contact={props?.contacts?.[note.author.substr(1)]}
            hideNicknames={props.hideNicknames}
            baseUrl={props.baseUrl}
          />
        );
      })}
    </Col>
  );
}

export default NotebookPosts;
