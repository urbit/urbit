import React, { useState, useEffect } from 'react';
import { Box, Text, Col, Anchor, Row, Action } from '@tlon/indigo-react';
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
import {useCopy} from '~/logic/lib/useCopy';
import {usePermalinkForGraph, getPermalinkForGraph} from '~/logic/lib/permalinks';
import {useQuery} from '~/logic/lib/useQuery';

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

const renderers = {
  link: ({ href, children }) => {
    return (
      <Anchor display="inline" target="_blank" href={href}>{children}</Anchor>
    )
  }
};

export function NoteContent({ body }) {
  return (

      <Box color="black" className="md" style={{ overflowWrap: 'break-word', overflow: 'hidden' }}>
        <ReactMarkdown source={body} linkTarget={'_blank'} renderers={renderers} />
      </Box>
  );

}

export function Note(props: NoteProps & RouteComponentProps) {
  const [deleting, setDeleting] = useState(false);

  const { association, notebook, note, ship, book, api, rootUrl, baseUrl, group } = props;

  const deletePost = async () => {
    setDeleting(true);
    const indices = [note.post.index];
    await api.graph.removeNodes(ship, book, indices);
    props.history.push(rootUrl);
  };

  const { query } = useQuery();
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
      <Link to={`${baseUrl}/edit`}>
        <Action>Update</Action>
      </Link>
    )
  };

  if (window.ship === note?.post?.author || ourRole === "admin") {
    adminLinks.push(
      <Action destructive onClick={deletePost}>
        Delete
      </Action>
    )
  };

  const permalink = getPermalinkForGraph(
    association.group,
    association.resource,
    `/${noteId.toString()}`
  );

  const { doCopy, copyDisplay } = useCopy(permalink, 'Copy Link');

  const windowRef = React.useRef(null);
  useEffect(() => {
    if (windowRef.current && !query.has('selected')) {
      windowRef.current.parentElement.scrollTop = 0;
    }
  }, [note, windowRef]);

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
          >
            <Row px="2" gapX="2" alignItems="flex-end">
              <Action bg="white" onClick={doCopy}>{copyDisplay}</Action>
              {adminLinks}
            </Row>
          </Author>
        </Row>
      </Col>
      <NoteContent body={body} />
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
