import React, { useState, useEffect } from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import ReactMarkdown from "react-markdown";
import bigInt from 'big-integer';

import { Link, RouteComponentProps } from "react-router-dom";
import { Spinner } from "~/views/components/Spinner";
import { Comments } from "~/views/components/Comments";
import { NoteNavigation } from "./NoteNavigation";
import GlobalApi from "~/logic/api/global";
import { getLatestRevision, getComments } from '~/logic/lib/publish';
import Author from "~/views/components/Author";
import { Contacts, GraphNode, Graph, LocalUpdateRemoteContentPolicy } from "~/types";

interface NoteProps {
  ship: string;
  book: string;
  note: GraphNode;
  notebook: Graph;
  contacts: Contacts;
  api: GlobalApi;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  rootUrl: string;
  baseUrl: string;
}

export function Note(props: NoteProps & RouteComponentProps) {
  const [deleting, setDeleting] = useState(false);

  const { notebook, note, contacts, ship, book, api, rootUrl, baseUrl } = props;
  const editCommentId = props.match.params.commentId;

  const deletePost = async () => {
    setDeleting(true);
    const indices = [note.post.index]
    await api.graph.removeNodes(ship, book, indices);
    props.history.push(rootUrl);
  };

  const comments = getComments(note);
  const [revNum, title, body, post] = getLatestRevision(note);

  const noteId = bigInt(note.post.index.split('/')[1]);

  let adminLinks: JSX.Element | null = null;
  if (window.ship === note?.post?.author) {
    adminLinks = (
      <Box display="inline-block" verticalAlign="middle">
        <Link to={`${baseUrl}/edit`}>
        <Text
          color="green"
          ml={2}
        >
          Update
        </Text>
      </Link>
        <Text
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
        <Text display="block" mb={2}>{title || ""}</Text>
        <Box display="flex">
          <Author
            hideNicknames={props?.hideNicknames}
            hideAvatars={props?.hideAvatars}
            ship={post?.author}
            contacts={contacts}
            date={post?.["time-sent"]}
          />
          <Text ml={2}>{adminLinks}</Text>
        </Box>
      </Col>
      <Box color="black" className="md" style={{ overflowWrap: "break-word" }}>
        <ReactMarkdown source={body} linkTarget={"_blank"} />
      </Box>
      <NoteNavigation
        notebook={notebook}
        noteId={noteId}
        ship={props.ship}
        book={props.book}
      />
      <Comments
        ship={ship}
        name={props.book}
        comments={comments}
        contacts={props.contacts}
        api={props.api}
        hideNicknames={props.hideNicknames}
        hideAvatars={props.hideAvatars}
        remoteContentPolicy={props.remoteContentPolicy}
        baseUrl={baseUrl}
        editCommentId={editCommentId}
        history={props.history}
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
