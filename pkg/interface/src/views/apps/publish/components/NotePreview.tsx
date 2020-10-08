import React from "react";
import { Col, Box } from "@tlon/indigo-react";
import { cite } from "~/logic/lib/util";
import { Note } from "~/types/publish-update";
import { Contact } from "~/types/contact-update";
import ReactMarkdown from "react-markdown";
import moment from "moment";
import { Link } from "react-router-dom";
import styled from "styled-components";

interface NotePreviewProps {
  host: string;
  book: string;
  note: Note;
  contact?: Contact;
  hideNicknames?: boolean;
}

const WrappedBox = styled(Box)`
  overflow-wrap: break-word;
`;

export function NotePreview(props: NotePreviewProps) {
  const { note, contact } = props;

  let name = note.author;
  if (contact && !props.hideNicknames) {
    name = contact.nickname.length > 0 ? contact.nickname : note.author;
  }
  if (name === note.author) {
    name = cite(note.author);
  }
  let comment = "No Comments";
  if (note["num-comments"] == 1) {
    comment = "1 Comment";
  } else if (note["num-comments"] > 1) {
    comment = `${note["num-comments"]} Comments`;
  }
  const date = moment(note["date-created"]).fromNow();
  const url = `${props.book}/note/${note["note-id"]}`;

  return (
    <Link to={url}>
      <Col mb={4}>
        <WrappedBox mb={1}>{note.title}</WrappedBox>
        <WrappedBox mb={1}>
          <ReactMarkdown
            unwrapDisallowed
            allowedTypes={["text", "root", "break", "paragraph"]}
            source={note.snippet}
          />
        </WrappedBox>
        <Box color="gray" display="flex">
          <Box
            mr={3}
            fontFamily={contact?.nickname && !props.hideNicknames ? "sans" : "mono"}
          >
            {name}
          </Box>
          <Box color={note.read ? "gray" : "green"} mr={3}>
            {date}
          </Box>
          <Box>{comment}</Box>
        </Box>
      </Col>
    </Link>
  );
}
