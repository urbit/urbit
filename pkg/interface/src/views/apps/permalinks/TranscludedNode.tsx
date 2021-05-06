import { Anchor, Box, Col, Icon, Row, Text } from '@tlon/indigo-react';
import { Association, GraphNode, Group, Post } from '@urbit/api';
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
  const idx = node?.post?.index?.slice(1).split('/') ?? [];

  switch (idx.length) {
    case 1:
    const [{ text }, link] = node.post.contents;
      if('reference' in link) {
        const permalink = referenceToPermalink(link).link;
        return <PermalinkEmbed transcluded={transcluded + 1} api={api} link={permalink} association={assoc} />;
      }

      return (
        <Box borderRadius="2" p="2" bg="scales.black05">
          <Anchor underline={false} target="_blank" color="black" href={link.url}>
            <Icon verticalAlign="bottom" mr="2" icon="ArrowExternal" />
            {text}
          </Anchor>
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
  const group = useGroupForAssoc(assoc)!;

  const comment = node.children?.peekLargest()![1]!;
  return (
    <Col>
      <Author
        p="2"
        showImage
        ship={comment.post.author}
        date={comment.post?.['time-sent']}
        group={group}
      />
      <Box p="2">
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
  const idx = node?.post?.index?.slice(1).split('/') ?? [];
  switch (idx.length) {
    case 1:
      const post = node.children
        ?.get(bigInt.one)
        ?.children?.peekLargest()?.[1]!;
      return (
        <Col color="black" gapY="2">
          <Author
            px="2"
            showImage
            ship={post.post.author}
            date={post.post?.['time-sent']}
            group={group}
          />
          <Text px="2" fontSize="2" fontWeight="medium">
            {post.post.contents[0]?.text}
          </Text>
          <Box p="2">
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
  group: Group;
}) {
  const { transcluded, post, group, api } = props;
  return (
    <Col>
      <Author
        p="2"
        showImage
        ship={post.author}
        date={post?.['time-sent']}
        group={group}
      />
      <Box p="2">
        <MentionText
          api={api}
          transcluded={transcluded}
          content={post.contents}
          group={group}
        />
      </Box>
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
  const { node, showOurContact, assoc, transcluded } = props;
  const group = useGroupForAssoc(assoc)!;
  switch (assoc.metadata.config.graph) {
    case 'chat':
      return (
        <Row width="100%" flexShrink={0} flexGrow={1} flexWrap="wrap">
          <ChatMessage
            width="100%"
            renderSigil
            transcluded={transcluded + 1}
            containerClass="items-top cf hide-child"
            association={assoc}
            group={group}
            groups={{}}
            msg={node.post}
            fontSize="0"
            ml="0"
            mr="0"
            showOurContact={showOurContact}
            pt="2"
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
        group={group}
        transcluded={transcluded}
      />)
      ;
    default:
      return null;
  }
}
