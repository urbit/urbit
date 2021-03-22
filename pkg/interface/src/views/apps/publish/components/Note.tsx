import React, { useState, useEffect } from 'react';
import { Box, Text, Col, Anchor, Row } from '@tlon/indigo-react';
import ReactMarkdown from 'react-markdown';
import bigInt from 'big-integer';

import { Link, RouteComponentProps } from 'react-router-dom';
import { Spinner } from '~/views/components/Spinner';
import { Comments } from '~/views/components/Comments';
import { NoteNavigation } from './NoteNavigation';
import GlobalApi from '~/logic/api/global';
import { getLatestRevision, getComments } from '~/logic/lib/publish';
import { roleForShip } from '~/logic/lib/group';
import Author from '~/views/components/Author';
import { Contacts, GraphNode, Graph, Association, Unreads, Group } from '@urbit/api';

interface NoteProps {
  ship: string;
  book: string;
  note: GraphNode;
  association: Association;
  notebook: Graph;
  api: GlobalApi;
  rootUrl: string;
  baseUrl: string;
  group: Group;
}

export function Note(props: NoteProps & RouteComponentProps) {
  const [deleting, setDeleting] = useState(false);

  const { notebook, note, ship, book, api, rootUrl, baseUrl, group } = props;
  const editCommentId = props.match.params.commentId;

  const renderers = {
    link: ({ href, children }) => {
      return (
        <Anchor display="inline" target="_blank" href={href}>{children}</Anchor>
      )
    }
  };

  const deletePost = async () => {
    setDeleting(true);
    const indices = [note.post.index];
    await api.graph.removeNodes(ship, book, indices);
    props.history.push(rootUrl);
  };

  const comments = getComments(note);
  const [revNum, title, body, post] = getLatestRevision(note);
  const index = note.post.index.split('/');

  const noteId = bigInt(index[1]);
  useEffect(() => {
    api.hark.markEachAsRead(props.association, '/',`/${index[1]}/1/1`, 'note', 'publish');
  }, [props.association, props.note]);

  let adminLinks: JSX.Element[] = [];
  const ourRole = roleForShip(group, window.ship);
  if (window.ship === note?.post?.author) {
    adminLinks.push(
        <Link
          style={{ 'display': 'inline-block' }}
          to={`${baseUrl}/edit`}
        >
          <Text
            color="blue"
            ml={2}
          >
            Update
          </Text>
      </Link>
    )
  };

  if (window.ship === note?.post?.author || ourRole === "admin") {
    adminLinks.push(
      <Text
        color="red"
        display='inline-block'
        ml={2}
        onClick={deletePost}
        style={{ cursor: 'pointer' }}
      >
        Delete
      </Text>
    )
  };

  const windowRef = React.useRef(null);
  useEffect(() => {
    if (windowRef.current) {
      windowRef.current.parentElement.scrollTop = 0;
    }
  }, [windowRef, note]);

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
      ref={windowRef}
    >
      <Link to={rootUrl}>
        <Text>{'<- Notebook Index'}</Text>
      </Link>
      <Col>
        <Text display="block" mb={2}>{title || ''}</Text>
        <Row alignItems="center">
          <Author
            showImage
            ship={post?.author}
            date={post?.['time-sent']}
            group={group}
          />
          <Text ml={1}>{adminLinks}</Text>
        </Row>
      </Col>
      <Box color="black" className="md" style={{ overflowWrap: 'break-word', overflow: 'hidden' }}>
        <ReactMarkdown source={body} linkTarget={'_blank'} renderers={renderers} />
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
        association={props.association}
        api={props.api}
        baseUrl={baseUrl}
        editCommentId={editCommentId}
        history={props.history}
        group={group}
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
