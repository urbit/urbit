/* eslint-disable no-case-declarations */
/* eslint-disable @typescript-eslint/no-non-null-asserted-optional-chain */
import { Anchor, Box, Col, Icon, Row, Text } from '@tlon/indigo-react';
import { Association, GraphConfig, GraphNode, Group, Post, ReferenceContent, TextContent, UrlContent } from '@urbit/api';
import bigInt from 'big-integer';
import React from 'react';
import { referenceToPermalink } from '~/logic/lib/permalinks';
import { getSnippet } from '~/logic/lib/publish';
import { useGroupForAssoc } from '~/logic/state/group';
import Author from '~/views/components/Author';
import { MentionText } from '~/views/components/MentionText';
import { GraphContent } from '~/views/landscape/components/Graph/GraphContent';
import ChatMessage from '../chat/components/ChatMessage';
import { NotePreviewContent } from '../publish/components/NotePreview';
import { PermalinkEmbed } from './embed';

function TranscludedLinkNode(props: {
  node: GraphNode;
  assoc: Association;
  transcluded: number;
}) {
  const { node, assoc, transcluded } = props;
  const idx = node?.post?.index?.slice(1)?.split('/') ?? [];

  if (typeof node?.post === 'string') {
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
      >
        <Text gray>This link has been deleted.</Text>
      </Box>
    );
  }

  switch (idx.length) {
    case 1:
      const [{ text }, link] = node.post.contents as [TextContent, UrlContent | ReferenceContent];
      if('reference' in link) {
        const permalink = referenceToPermalink(link).link;
        return <PermalinkEmbed transcluded={transcluded + 1} link={permalink} association={assoc} />;
      }

      return (
        <Box>
          <Author
            pt='12px'
            pl='12px'
            mt='6px'
            size={24}
            sigilPadding='6'
            showImage
            ship={node.post.author}
            date={node.post?.['time-sent']}
          />
          <Box
            borderRadius='2'
            mt='3'
            ml='44px'
            mr='3'
            p='2'
            display='inline-block'
            bg='scales.black05'
          >
            <Anchor
              underline={false}
              target='_blank'
              color='black'
              href={link.url}
            >
              <Icon verticalAlign='bottom' mr='2' icon='ArrowExternal' />
              {text}
            </Anchor>
          </Box>
        </Box>
      );

    case 2:
      return (
        <TranscludedComment
          transcluded={transcluded}
          node={node}
          assoc={assoc}
        />
      );
    default:
      return null;
  }
}

function TranscludedComment(props: {
  node: GraphNode;
  assoc: Association;
  transcluded: number;
}) {
  const { assoc, node, transcluded } = props;

  if (typeof node?.post === 'string') {
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
      >
        <Text gray>This comment has been deleted.</Text>
      </Box>
    );
  }

  const group = useGroupForAssoc(assoc)!;

  const comment = node.children?.peekLargest()![1]!;
  return (
    <Col>
      <Author
        pt='12px'
        pl='12px'
        mt='6px'
        size={24}
        sigilPadding='6'
        showImage
        ship={comment.post.author}
        date={comment.post?.['time-sent']}
        group={group}
      />
      <Box pl="44px" pt='2'>
        <GraphContent
          transcluded={transcluded}
          contents={comment.post.contents}
          showOurContact={false}
        />
      </Box>
    </Col>
  );
}

function TranscludedPublishNode(props: {
  node: GraphNode;
  assoc: Association;
  transcluded: number;
}) {
  const { node, assoc, transcluded } = props;
  const group = useGroupForAssoc(assoc)!;

  if (typeof node?.post === 'string') {
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
      >
        <Text gray>This note has been deleted.</Text>
      </Box>
    );
  }

  const idx = node?.post?.index?.slice(1)?.split('/') ?? [];
  switch (idx.length) {
    case 1:
      const post = node.children
        ?.get(bigInt.one)
        ?.children?.peekLargest()?.[1]!;
      return (
        <Col color="black">
          <Author
            pl='12px'
            pt='12px'
            mt='6px'
            size={24}
            sigilPadding='6'
            showImage
            ship={post.post.author}
            date={post.post?.['time-sent']}
            group={group}
          />
          <Text mt='3' pl='44px' fontSize="2" fontWeight="medium">
            {(post.post.contents[0] as TextContent)?.text}
          </Text>
          <Box pl="44px" pr='3'>
            <NotePreviewContent
              snippet={getSnippet(post?.post.contents.slice(1))}
            />
          </Box>
        </Col>
      );

    case 3:
      return (
        <TranscludedComment
          transcluded={transcluded}
          node={node}
          assoc={assoc}
        />
      );
    default:
      return null;
  }
}

export function TranscludedPost(props: {
  post: Post;
  transcluded: number;
  commentsCount?: number;
  group: Group;
}) {
  const { transcluded, post, group, commentsCount } = props;

  if (typeof post === 'string') {
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
      >
        <Text gray>This post has been deleted.</Text>
      </Box>
    );
  }

  return (
    <Col>
      <Author
        pt='12px'
        pl='12px'
        mt='6px'
        size={24}
        sigilPadding='6'
        showImage
        ship={post.author}
        date={post?.['time-sent']}
        group={group}
      />
      <Box pl='44px' pt='3' pr='3'>
        <MentionText
          transcluded={transcluded}
          content={post.contents}
          group={group}
        />
      </Box>
      {commentsCount >= 1 ?
        <Box pl='44px' pt='2' pr='3'>
          <Text>
            {commentsCount} {commentsCount === 1 ? 'reply' : 'replies'}
          </Text>
        </Box>
      : null}
    </Col>
  );
}

export function TranscludedNode(props: {
  assoc: Association;
  node: GraphNode;
  transcluded: number;
  showOurContact?: boolean;
}) {
  const { node, showOurContact, assoc, transcluded } = props;
  const group = useGroupForAssoc(assoc)!;

  if (
    typeof node?.post === 'string' &&
    (assoc.metadata.config as GraphConfig).graph === 'chat'
  ) {
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
      >
        <Text gray>This message has been deleted.</Text>
      </Box>
    );
  }

  switch ((assoc.metadata.config as GraphConfig).graph) {
    case 'chat':
      return (
        <Row width="100%" flexShrink={0} flexGrow={1} flexWrap="wrap">
          <ChatMessage
            renderSigil
            transcluded={transcluded + 1}
            className="items-top cf hide-child"
            // @ts-ignore isn't forwarding props to memo
            association={assoc}
            msg={node.post}
            fontSize={0}
            showOurContact={showOurContact}
            mt='0'
            isReply
          />
        </Row>
      );
    case 'publish':
      return <TranscludedPublishNode {...props} />;
    case 'link':
      return <TranscludedLinkNode {...props} />;
    case 'post':
      return (
        <TranscludedPost
          post={node.post}
          commentsCount={Object.keys(node.children.root).length}
          group={group}
          transcluded={transcluded}
        />
      );
    default:
      return null;
  }
}
