import React from 'react';
import {
  Icon,
  Row,
  Text,
  Col,
  Box,
  CenterProps
} from '@tlon/indigo-react';
import { AUDIO_REGEX, IMAGE_REGEX, validOembedCheck } from '~/views/components/RemoteContent';
import { AudioPlayer } from '~/views/components/AudioPlayer';
import { useHistory } from 'react-router';
import { useHovering } from '~/logic/lib/util';
import { useEmbed } from '~/logic/state/embed';
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
import AsyncFallback from '~/views/components/AsyncFallback';

export interface LinkBlockItemProps {
  node: GraphNode;
  size?: CenterProps['height'];
  border?: CenterProps['border'];
  objectFit?: string;
  summary?: boolean;
}

export const LinkBlockItem = (props: LinkBlockItemProps & CenterProps) => {
  const { node, ...rest } = props;
  const { post } = node;
  const { contents } = post;

  const [{ text: title }, ...content] = contents as [
    TextContent,
    UrlContent | ReferenceContent
  ];
  let url = '';
  if ('url' in content?.[0]) {
    url = content[0].url;
  }

  return(
    <AsyncFallback fallback={<RemoteContentEmbedFallback url={url} />}>
      <LinkBlockItemInner
        node={node}
        {...rest}
      />
    </AsyncFallback>
  );
}

function LinkBlockItemInner(props: LinkBlockItemProps & CenterProps) {
  const { node, summary, m, border = 1, objectFit, ...rest } = props;
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
  const oembed = useEmbed(url);
  const isOembed = validOembedCheck(oembed, url);

  const history = useHistory();
  const { hovering, bind } = useHovering();
  const onClick = () => {
    const { pathname, search } = history.location;
    history.push(`${pathname}/index${index}${search}`);
  };
  return (
    <Box
      onClick={onClick}
      position="relative"
      m={m}
      border={border}
      borderColor="lightGray"
      borderRadius="1"
      {...rest}
      {...bind}
    >
      <Col height="100%" justifyContent="center" alignItems="center">
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
            <RemoteContentImageEmbed
              url={url}
              tall
              stretch
              objectFit={objectFit ? objectFit : "cover"}
            />
          ) : isAudio ? (
            <AudioPlayer title={title} url={url} />
          ) : isOembed ? (
            <RemoteContentOembed tall={!summary} renderUrl={false} url={url} thumbnail={summary} oembed={oembed} />
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
      </Col>
    </Box>
  );
}
