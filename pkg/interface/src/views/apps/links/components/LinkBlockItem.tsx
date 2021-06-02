import React from 'react';
import {
  BaseImage,
  Icon,
  Center,
  Row,
  Text,
  BoxProps,
  Col,
  Box,
} from '@tlon/indigo-react';
import { AUDIO_REGEX, IMAGE_REGEX } from '~/views/components/RemoteContent';
import { AudioPlayer } from '~/views/components/AudioPlayer';
import { useHistory } from 'react-router';
import { useHovering } from '~/logic/lib/util';
import Author from '~/views/components/Author';
import { GraphNode, Post, TextContent, UrlContent } from '@urbit/api';

export interface LinkBlockItemProps {
  node: GraphNode;
  size?: BoxProps['height'];
  m?: BoxProps['m'];
  border?: BoxProps['border'];
  summary?: boolean;
}

function getYoutubeId(str: string): string | null {
  const youtube = str.match(/youtube\.com.*(\?v=|\/embed\/)(.{11})/);
  if (!youtube) {
    return null;
  }
  return youtube.pop();
}

export function LinkBlockItem(props: LinkBlockItemProps) {
  const { node, summary, size = '256px', m, border = 1 } = props;
  const { post, children } = node;
  const { contents, index, author } = post;
  const [{ text: title }, { url }] = contents as [TextContent, UrlContent];

  const isImage = IMAGE_REGEX.test(url);
  const isAudio = AUDIO_REGEX.test(url);
  const youtube = getYoutubeId(url);
  const history = useHistory();
  const { hovering, bind } = useHovering();
  const onClick = () => {
    history.push(`${history.location.pathname}/index${index}`);
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
      {...bind}
    >
      {isImage ? (
        <BaseImage
          style={{ objectFit: 'contain' }}
          height="100%"
          src={url}
          width="100%"
        />
      ) : isAudio ? (
        <AudioPlayer title={title} url={url} />
      ) : youtube ? (
        <BaseImage
          style={{ objectFit: 'contain' }}
          height="100%"
          src={`https://img.youtube.com/vi/${youtube}/${0}.jpg`}
          width="100%"
        />
      ) : (
        <Row overflow="hidden" gapX="2" alignItems="center" p="2">
          <Icon color="gray" icon="ArrowExternal" />
          <Text
            gray
            overflow="hidden"
            whiteSpace="nowrap"
            textOverflow="ellipsis"
          >
            {url}
          </Text>
        </Row>
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
