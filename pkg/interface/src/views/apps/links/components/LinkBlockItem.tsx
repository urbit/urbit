import React from 'react';
import {
  BaseImage,
  Icon,
  Center,
  Row,
  Text
} from '@tlon/indigo-react';
import { AUDIO_REGEX, IMAGE_REGEX } from '~/views/components/RemoteContent';
import { AudioPlayer } from '~/views/components/AudioPlayer';

export interface LinkBlockItemProps {
  url: string;
  title?: string;
}

function getYoutubeId(str: string): string | null {
  const youtube = str.match(/youtube\.com.*(\?v=|\/embed\/)(.{11})/);
  if(!youtube) {
    return null;
  }
  return youtube.pop();
}

export function LinkBlockItem(props: LinkBlockItemProps) {
  const { url, title } = props;

  const isImage = IMAGE_REGEX.test(url);
  const isAudio = AUDIO_REGEX.test(url);
  const youtube = getYoutubeId(url);
  return (
    <Center
      border="1"
      borderColor="lightGray"
      borderRadius="1"
      height="256px"
      width="256px"
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
    </Center>
  );
}
