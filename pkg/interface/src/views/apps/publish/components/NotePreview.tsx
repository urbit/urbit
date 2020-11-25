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
  getSnippet
} from '~/logic/lib/publish';
import GlobalApi from '~/logic/api/global';
import ReactMarkdown from 'react-markdown';

interface NotePreviewProps {
  host: string;
  book: string;
  node: GraphNode;
  hideAvatars?: boolean;
  hideNicknames?: boolean;
  baseUrl: string;
  contacts: Contacts;
  api: GlobalApi;
  group: Group;
}

const WrappedBox = styled(Box)`
  overflow-wrap: break-word;
`;

export function NotePreview(props: NotePreviewProps) {
  const { node, contacts, hideAvatars, hideNicknames, group } = props;
  const { post } = node;
  if (!post) {
    return null;
  }

  const numComments = getComments(node).children.size;
  const url = `${props.baseUrl}/note/${post.index.split('/')[1]}`;

  // stubbing pending notification-store
  const isRead = true;

  const [rev, title, body] = getLatestRevision(node);

  const snippet = getSnippet(body);

  return (
    <Box width='100%'>
      <Link to={url}>
        <Col
          lineHeight='tall'
          width='100%'
          color={isRead ? 'washedGray' : 'blue'}
          border={1}
          borderRadius={2}
          alignItems='flex-start'
          overflow='hidden'
          p='2'
        >
          <WrappedBox mb={2}><Text bold fontSize='0'>{title}</Text></WrappedBox>
          <WrappedBox>
          <Text fontSize='14px'>
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
          hideAvatars={hideAvatars || false}
          hideNicknames={hideNicknames || false}
          group={group}
          api={props.api}
        />
        <Box ml="auto" mr={1}>
          <Link to={url}>
            <Box display='flex'>
              <Icon color='blue' icon='Chat' />
              <Text color='blue' ml={1}>{numComments}</Text>
            </Box>
          </Link>
        </Box>
      </Row>
    </Box>
  );
}
