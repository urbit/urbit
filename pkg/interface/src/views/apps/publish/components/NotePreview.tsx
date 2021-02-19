import React from 'react';
import { Link } from 'react-router-dom';
import styled from 'styled-components';
import { Col, Row, Box, Text, Icon, Image } from '@tlon/indigo-react';

import Author from '~/views/components/Author';
import { GraphNode } from '~/types/graph-update';
import { Contacts, Group } from '~/types';
import {
  getComments,
  getLatestRevision,
  getSnippet,
} from "~/logic/lib/publish";
import {Unreads} from "~/types";
import GlobalApi from '~/logic/api/global';
import ReactMarkdown from 'react-markdown';

interface NotePreviewProps {
  host: string;
  book: string;
  node: GraphNode;
  baseUrl: string;
  unreads: Unreads;
  contacts: Contacts;
  api: GlobalApi;
  group: Group;
}

const WrappedBox = styled(Box)`
  overflow-wrap: break-word;
`;

export function NotePreview(props: NotePreviewProps) {
  const { node, contacts, group } = props;
  const { post } = node;
  if (!post) {
    return null;
  }

  const numComments = getComments(node).children.size;
  const noteId = post.index.split('/')[1];
  const url = `${props.baseUrl}/note/${noteId}`;

  const [rev, title, body, content] = getLatestRevision(node);
  const appPath = `/ship/${props.host}/${props.book}`;
  const isUnread = props.unreads.graph?.[appPath]?.['/']?.unreads?.has(`/${noteId}/1/1`);

  const snippet = getSnippet(body);

  const commColor = (props.unreads.graph?.[appPath]?.[`/${noteId}`]?.unreads ?? 0) > 0 ? 'blue' : 'gray';
  return (
    <Box width='100%'>
      <Link to={url}>
        <Col
          lineHeight='tall'
          width='100%'
          color={!isUnread ? 'washedGray' : 'blue'}
          border={1}
          borderRadius={2}
          alignItems='flex-start'
          overflow='hidden'
          p='2'
        >
          <WrappedBox mb={2}><Text bold>{title}</Text></WrappedBox>
          <WrappedBox>
          <Text fontSize='14px' lineHeight='tall'>
            <ReactMarkdown
              unwrapDisallowed
              allowedTypes={['text', 'root', 'break', 'paragraph', 'image']}
              renderers={{
                image: props => <Image src={props.src} maxHeight='300px' style={{ objectFit: 'cover' }} />
              }}
              source={snippet}
            />
            </Text>
          </WrappedBox>
        </Col>
      </Link>
      <Row minWidth='0' flexShrink={0} width="100%" justifyContent="space-between" py={3} bg="white">
        <Author
          showImage
          contacts={contacts}
          ship={post?.author}
          date={post?.['time-sent']}
          group={group}
          unread={isUnread}
          api={props.api}
        />
        <Box ml="auto" mr={1}>
          <Link to={url}>
            <Box display='flex'>
              <Icon color={commColor} icon='Chat' />
              <Text color={commColor} ml={1}>{numComments}</Text>
            </Box>
          </Link>
        </Box>
      </Row>
    </Box>
  );
}
