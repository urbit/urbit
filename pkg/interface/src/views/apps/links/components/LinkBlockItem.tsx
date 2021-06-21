import React from 'react';
import {
  Icon,
  Center,
  Row,
  Text,
  Col,
  Box,
  CenterProps
} from '@tlon/indigo-react';
import { hasProvider } from 'oembed-parser';
import { AUDIO_REGEX, IMAGE_REGEX } from '~/views/components/RemoteContent';
import { AudioPlayer } from '~/views/components/AudioPlayer';
import { useHistory } from 'react-router';
import { useHovering } from '~/logic/lib/util';
import Author from '~/views/components/Author';
import {
  GraphNode,
  ReferenceContent,
  TextContent,
  UrlContent
} from '@urbit/api';
import {
  RemoteContentEmbedFallback,
  RemoteContentImageEmbed,
  RemoteContentOembed,
  RemoteContentPermalinkEmbed
} from '~/views/components/RemoteContent/embed';
import { PermalinkEmbed } from '../../permalinks/embed';
import { referenceToPermalink } from '~/logic/lib/permalinks';

export interface LinkBlockItemProps {
  node: GraphNode;
  size?: CenterProps['height'];
  border?: CenterProps['border'];
  summary?: boolean;
}

export function LinkBlockItem(props: LinkBlockItemProps & CenterProps) {
  const { node, summary, size, m, border = 1, ...rest } = props;
  const { post, children } = node;
  const { contents, index, author } = post;

  const [{ text: title }, ...content] = contents as [
    TextContent,
    UrlContent | ReferenceContent
  ];
  let url = '';
  if ('url' in content?.[0]) {
    url = content[0].url;
  }

  const isReference = 'reference' in content[0];

  const isImage = IMAGE_REGEX.test(url);
  const isAudio = AUDIO_REGEX.test(url);

  const isOembed = hasProvider(url);
  const history = useHistory();
  const { hovering, bind } = useHovering();
  const onClick = () => {
    const { pathname, search } = history.location;
    history.push(`${pathname}/index${index}${search}`);
  };
  return (
    <Center
      onClick={onClick}
      border={border}
      borderColor="lightGray"
      position="relative"
      borderRadius="1"
      height={size}
      width={size}
      m={m}
      {...rest}
      {...bind}
    >
      {isReference ? (
        summary ? (
          <RemoteContentPermalinkEmbed
            reference={content[0] as ReferenceContent}
          />
        ) : (
          <PermalinkEmbed
            link={referenceToPermalink(content[0] as ReferenceContent).link}
            transcluded={0}
          />
        )
      ) : isImage ? (
        <RemoteContentImageEmbed url={url} />
      ) : isAudio ? (
        <AudioPlayer title={title} url={url} />
      ) : isOembed ? (
        <RemoteContentOembed url={url} thumbnail={summary} />
      ) : (
        <RemoteContentEmbedFallback url={url} />
      )}
      <Box
        backgroundColor="white"
        display={summary && hovering ? 'block' : 'none'}
        width="100%"
        height="64px"
        position="absolute"
        left="0"
        bottom="0"
      >
        <Col width="100%" height="100%" p="2" justifyContent="space-between">
          <Row justifyContent="space-between" width="100%">
            <Text textOverflow="ellipsis" whiteSpace="nowrap" overflow="hidden">
              {title}
            </Text>
            <Row gapX="1" alignItems="center">
              <Icon icon="Chat" color="black" />
              <Text>{children.size}</Text>
            </Row>
          </Row>
          <Row width="100%">
            <Author ship={author} date={post['time-sent']} showImage></Author>
          </Row>
        </Col>
      </Box>
    </Center>
  );
}
