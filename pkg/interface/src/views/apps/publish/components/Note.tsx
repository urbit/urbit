import { Action, Anchor, Box, Col, Row, Text } from '@tlon/indigo-react';
import { Association, Graph, GraphNode, Group } from '@urbit/api';
import bigInt from 'big-integer';
import React, { useEffect, useState } from 'react';
import ReactMarkdown from 'react-markdown';
import { Link, RouteComponentProps } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { roleForShip } from '~/logic/lib/group';
import { getPermalinkForGraph } from '~/logic/lib/permalinks';
import { getComments, getLatestRevision } from '~/logic/lib/publish';
import { useCopy } from '~/logic/lib/useCopy';
import { useQuery } from '~/logic/lib/useQuery';
import Author from '~/views/components/Author';
import { Comments } from '~/views/components/Comments';
import { Spinner } from '~/views/components/Spinner';
import { NoteNavigation } from './NoteNavigation';

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
    );
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
    await api.graph.removePosts(ship, book, indices);
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

  const adminLinks: JSX.Element[] = [];
  const ourRole = roleForShip(group, window.ship);
  if (window.ship === note?.post?.author) {
    adminLinks.push(
      <Link to={`${baseUrl}/edit`}>
        <Action backgroundColor="white">Update</Action>
      </Link>
    );
  }

  if (window.ship === note?.post?.author || ourRole === 'admin') {
    adminLinks.push(
      <Action backgroundColor="white" destructive onClick={deletePost}>
        Delete
      </Action>
    );
  }

  const permalink = getPermalinkForGraph(
    association.group,
    association.resource,
    `/${noteId.toString()}`
  );

  const { doCopy, copyDisplay } = useCopy(permalink, 'Copy Link');

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
        <Text>{'<- Notebook Index'}</Text>
      </Link>
      <Col>
        <Text display="block" mb={2}>{title || ''}</Text>
        <Row alignItems="center">
          <Author
            showImage
            isRelativeTime
            ship={post?.author}
            date={post?.['time-sent']}
            group={group}
          >
            <Row px="2" gapX="2" alignItems="flex-end" height="14px">
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
