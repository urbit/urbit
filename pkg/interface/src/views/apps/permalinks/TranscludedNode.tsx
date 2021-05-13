import { Anchor, Box, Col, Icon, Row, Text } from '@tlon/indigo-react';
import { Association, GraphConfig, GraphNode, Group, Post, ReferenceContent, TextContent, UrlContent } from '@urbit/api';
import bigInt from 'big-integer';
import React from 'react';
import GlobalApi from '~/logic/api/global';
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
  api: GlobalApi;
}) {
  const { node, api, assoc, transcluded } = props;
  const idx = node?.post?.index?.slice(1)?.split('/') ?? [];

  if (typeof node?.post === 'string') {
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
        className='deleted-link'
      >
        <Text gray>This link has been deleted.</Text>
      </Box>
    )
  }

  switch (idx.length) {
    case 1:
      const [{ text }, link] = node.post.contents as [TextContent, UrlContent | ReferenceContent];
      if('reference' in link) {
        const permalink = referenceToPermalink(link).link;
        return <PermalinkEmbed transcluded={transcluded + 1} api={api} link={permalink} association={assoc} />;
      }

      return (
        <Box className='transcluded-link'>
          <Author
            pt='12px'
            pl='12px'
            size='24'
            sigilPadding='6'
            showImage
            ship={node.post.author}
            date={node.post?.['time-sent']}
          />
          <Box
            borderRadius='2'
            mt='1'
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
          api={api}
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
  api: GlobalApi;
  transcluded: number;
}) {
  const { assoc, node, api, transcluded } = props;

  if (typeof node?.post === 'string') {
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
        className='deleted-comment'
      >
        <Text gray>This comment has been deleted.</Text>
      </Box>
    )
  }

  const group = useGroupForAssoc(assoc)!;

  const comment = node.children?.peekLargest()![1]!;
  return (
    <Col className='transcluded-comment'>
      <Author
        pt='12px'
        pl='12px'
        size='24'
        sigilPadding='6'
        showImage
        ship={comment.post.author}
        date={comment.post?.['time-sent']}
        group={group}
      />
      <Box pl="44px" pt='1'>
        <GraphContent
          api={api}
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
  api: GlobalApi;
  transcluded: number;
}) {
  const { node, assoc, transcluded, api } = props;
  const group = useGroupForAssoc(assoc)!;

  if (typeof node?.post === 'string') {
    console.log(node)
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
        className='deleted-note'
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
        <Col color="black" gapY={2} className='transcluded-note'>
          <Author
            pl='12px'
            pt='12px'
            size='24'
            sigilPadding='6'
            showImage
            ship={post.post.author}
            date={post.post?.['time-sent']}
            group={group}
          />
          <Text pl='44px' fontSize="2" fontWeight="medium">
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
          api={api}
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
  api: GlobalApi;
  transcluded: number;
  commentsCount?: number;
  group: Group;
}) {
  const { transcluded, post, group, commentsCount, api } = props;

  if (typeof post === 'string') {
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
        className='deleted-post'
      >
        <Text gray>This post has been deleted.</Text>
      </Box>
    )
  }

  return (
    <Col className='transcluded-post'>
      <Author
        pt='12px'
        pl='12px'
        size='24'
        sigilPadding='6'
        showImage
        ship={post.author}
        date={post?.['time-sent']}
        group={group}
      />
      <Box pl='44px' pt='2' pr='3'>
        <MentionText
          api={api}
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
  api: GlobalApi;
  showOurContact?: boolean;
}) {
  const { node, showOurContact, assoc, transcluded, api } = props;
  const group = useGroupForAssoc(assoc)!;

  if (
    typeof node?.post === "string" &&
    assoc.metadata.config.graph === "chat"
  ) {
    return (
      <Box
        mx="12px"
        mt="12px"
        p="2"
        backgroundColor="washedGray"
        borderRadius="2"
        className='deleted-message'
      >
        <Text gray>This message has been deleted.</Text>
      </Box>
    );
  }

  switch ((assoc.metadata.config as GraphConfig).graph) {
    case 'chat':
      return (
        <Row
          width="100%"
          flexShrink={0}
          flexGrow={1}
          flexWrap="wrap"
          className='transcluded-node'
        >
          <ChatMessage
            renderSigil
            transcluded={transcluded + 1}
            className="items-top cf hide-child"
            association={assoc}
            msg={node.post}
            fontSize={0}
            showOurContact={showOurContact}
            api={api}
            mt='0'
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
        api={props.api}
        post={node.post}
        commentsCount={Object.keys(node.children.root).length}
        group={group}
        transcluded={transcluded}
      />)
      ;
    default:
      return null;
  }
}
