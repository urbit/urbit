import React, { useState, useEffect } from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import ReactMarkdown from "react-markdown";
import { Link, RouteComponentProps } from "react-router-dom";
import { Spinner } from "~/views/components/Spinner";
import { Comments } from "./Comments";
import { NoteNavigation } from "./NoteNavigation";
import GlobalApi from "~/logic/api/global";
import { Author } from "./Author";
import { Contacts, GraphNode, Graph} from "~/types";

interface NoteProps {
  ship: string;
  book: string;
  note: GraphNode;
  notebook: Graph;
  contacts: Contacts;
  api: GlobalApi;
  hideAvatars: boolean;
  hideNicknames: boolean;
}

export function Note(props: NoteProps & RouteComponentProps) {
  const [deleting, setDeleting] = useState(false);
  const { contacts, ship, notebook, book, api, note } = props;

  const baseUrl = `/~publish/notebook/ship/${props.ship}/${props.book}`;

  const deletePost = async () => {
    setDeleting(true);
    const indices = [note.post.index]
    await api.graph.removeNodes(ship, book, indices);
    props.history.push(baseUrl);
  };

  const comments = note?.children
  const file = note?.post?.contents[1]?.text || "";
  const newfile = file ? file.slice(file.indexOf(";>") + 2) : "";

  const noteId = parseInt(note.post.index.split('/')[1], 10);

  let adminLinks: JSX.Element | null = null;
  if (window.ship === note?.post?.author) {
    adminLinks = (
      <Box display="inline-block">
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
      display="grid"
      gridTemplateColumns="1fr"
      gridAutoRows="min-content"
      maxWidth="500px"
      width="100%"
      gridRowGap={4}
      mx="auto"
    >
      <Link to={baseUrl}>
        <Text>{"<- Notebook Index"}</Text>
      </Link>
      <Col>
        <Text display="block" mb={2}>{note?.post?.contents[0]?.text || ""}</Text>
        <Box display="flex">
          <Author
            hideNicknames={props.hideNicknames}
            hideAvatars={props.hideAvatars}
            ship={note?.post?.author}
            contacts={contacts}
            date={note?.post?.["time-sent"]}
          />
          <Text ml={2}>{adminLinks}</Text>
        </Box>
      </Col>
      <Box color="black" className="md" style={{ overflowWrap: "break-word" }}>
        <ReactMarkdown source={newfile} linkTarget={"_blank"} />
      </Box>
      <NoteNavigation
        notebook={notebook}
        noteId={noteId}
        ship={props.ship}
        book={props.book}
      />
      <Comments
        ship={ship}
        book={props.book}
        note={props.note}
        comments={comments}
        contacts={props.contacts}
        api={props.api}
        hideNicknames={props.hideNicknames}
        hideAvatars={props.hideAvatars}
      />
      <Spinner
        text="Deleting post..."
        awaiting={deleting}
        classes="absolute bottom-1 right-1 ba b--gray1-d pa2"
      />
    </Box>
  );
}

export default Note;
