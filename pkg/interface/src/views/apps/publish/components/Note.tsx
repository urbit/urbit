import React, { useState, useEffect } from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import ReactMarkdown from "react-markdown";
import { Link, RouteComponentProps } from "react-router-dom";
import { Spinner } from "~/views/components/Spinner";
import { Comments } from "./Comments";
import { NoteNavigation } from "./NoteNavigation";
import {
  NoteId,
  Note as INote,
  Notebook,
} from "~/types/publish-update";
import { Contacts } from "~/types/contact-update";
import GlobalApi from "~/logic/api/global";
import { Author } from "./Author";
import { LocalUpdateRemoteContentPolicy } from "~/types";

interface NoteProps {
  ship: string;
  book: string;
  noteId: NoteId;
  note: INote;
  notebook: Notebook;
  contacts: Contacts;
  api: GlobalApi;
  hideAvatars: boolean;
  hideNicknames: boolean;
  baseUrl?: string;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
}

export function Note(props: NoteProps & RouteComponentProps) {
  const [deleting, setDeleting] = useState(false);
  const { notebook, note, contacts, ship, book, noteId, api, baseUrl } = props;
  useEffect(() => {
    api.publish.readNote(ship.slice(1), book, noteId);
    api.publish.fetchNote(ship, book, noteId);
  }, [ship, book, noteId]);

  const rootUrl = props.baseUrl || '/~404';

  const deletePost = async () => {
    setDeleting(true);
    await api.publish.delNote(ship.slice(1), book, noteId);
    props.history.push(rootUrl);
  };

  const comments = note?.comments || [];
  const file = note?.file;
  const newfile = file ? file.slice(file.indexOf(";>") + 2) : "";

  let editPost: JSX.Element | null = null;
  const editUrl = props.location.pathname + "/edit";
  if (`~${window.ship}` === note?.author) {
    editPost = (
      <Box display="inline-block">
        <Link to={editUrl}>
          <Text color="green">Edit</Text>
        </Link>
        <Text
          className="dib f9 red2 ml2 pointer"
          color="red"
          ml={2}
          onClick={deletePost}
          css={{ cursor: "pointer" }}
        >
          Delete
        </Text>
      </Box>
    );
  }

  return (
    <Box
      my={3}
      px={3}
      display="grid"
      gridTemplateColumns="1fr"
      gridAutoRows="min-content"
      maxWidth="500px"
      width="100%"
      gridRowGap={4}
      mx="auto"
    >
      <Link to={rootUrl}>
        <Text>{"<- Notebook Index"}</Text>
      </Link>
      <Col>
        <Text display="block" mb={2}>{note?.title || ""}</Text>
        <Box display="flex">
          <Author
            hideNicknames={props?.hideNicknames}
            hideAvatars={props?.hideAvatars}
            ship={note?.author}
            contacts={contacts}
            date={note?.["date-created"]}
          />
          <Text ml={2}>{editPost}</Text>
        </Box>
      </Col>
      <Box color="black" className="md" style={{ overflowWrap: "break-word" }}>
        <ReactMarkdown source={newfile} linkTarget={"_blank"} />
      </Box>
      <NoteNavigation
        notebook={notebook}
        prevId={note?.["prev-note"] || undefined}
        nextId={note?.["next-note"] || undefined}
        ship={ship}
        book={book}
      />
      {notebook?.comments && (
        <Comments
          ship={ship}
          book={book}
          noteId={noteId}
          note={note}
          comments={comments}
          numComments={note?.["num-comments"]}
          contacts={contacts}
          api={api}
          hideNicknames={props?.hideNicknames}
          hideAvatars={props?.hideAvatars}
          remoteContentPolicy={props?.remoteContentPolicy}
        />
      )}
      <Spinner
        text="Deleting post..."
        awaiting={deleting}
        classes="absolute bottom-1 right-1 ba b--gray1-d pa2"
      />
    </Box>
  );
}

export default Note;
