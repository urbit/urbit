import { hasProvider } from 'oembed-parser';
import React from 'react';
import useSettingsState from '~/logic/state/settings';
import { RemoteContentPolicy } from '~/types/local-update';
import {
  RemoteContentAudioEmbed,
  RemoteContentImageEmbed,
  RemoteContentOembed,
  RemoteContentVideoEmbed
} from './embed';
import { TruncatedText } from '~/views/components/TruncatedText';
import { RemoteContentWrapper } from './wrapper';

export interface RemoteContentProps {
  url: string;
  text?: string;
  unfold?: boolean;
  renderUrl?: boolean;
  remoteContentPolicy?: RemoteContentPolicy;
  embedRef?: (el: HTMLDivElement | null ) => void;
  imageProps?: any;
  audioProps?: any;
  videoProps?: any;
  oembedProps?: any;
  textProps?: any;
  style?: any;
  transcluded?: any;
  className?: string;
  tall?: boolean;
}

export const IMAGE_REGEX = new RegExp(
  /(jpg|img|png|gif|tiff|jpeg|webp|webm|svg)$/i
);
export const AUDIO_REGEX = new RegExp(/(mp3|wav|ogg|m4a)$/i);
export const VIDEO_REGEX = new RegExp(/(mov|mp4|ogv)$/i);

const emptyRef = () => {};
export function RemoteContent(props: RemoteContentProps) {
  const {
    url,
    embedRef = emptyRef,
    text,
    transcluded,
    tall = false,
    renderUrl = true,
    imageProps = {},
    audioProps = {},
    videoProps = {},
    textProps = {}
  } = props;

  const remoteContentPolicy = useSettingsState(s => s.remoteContentPolicy);
  const isImage = IMAGE_REGEX.test(url);
  const isAudio = AUDIO_REGEX.test(url);
  const isVideo = VIDEO_REGEX.test(url);
  const isOembed = hasProvider(url);
  const wrapperProps = {
    url,
    tall,
    embedOnly: !renderUrl
  };

  if (isImage && remoteContentPolicy.imageShown) {
    return (
      <RemoteContentWrapper {...wrapperProps} noOp={transcluded}>
        <RemoteContentImageEmbed url={url} {...imageProps} />
      </RemoteContentWrapper>
    );
  } else if (isAudio && remoteContentPolicy.audioShown) {
    return (
      <RemoteContentWrapper
        {...wrapperProps}
        detail={<RemoteContentAudioEmbed url={url} {...audioProps} />}
      >
        <TruncatedText {...textProps}>{url}</TruncatedText>
      </RemoteContentWrapper>
    );
  } else if (isVideo && remoteContentPolicy.videoShown) {
    return (
      <RemoteContentWrapper
        {...wrapperProps}
        detail={
          <RemoteContentVideoEmbed url={url} {...videoProps} />
        }
      >
        <TruncatedText {...textProps}>{url}</TruncatedText>
      </RemoteContentWrapper>
    );
  } else if (isOembed && remoteContentPolicy.oembedShown) {
    return (
      <RemoteContentWrapper
        {...wrapperProps}
        detail={(
          <RemoteContentOembed ref={embedRef} url={url} renderUrl={renderUrl} />
        )}
      >
        <TruncatedText {...textProps}>{url}</TruncatedText>
      </RemoteContentWrapper>
    );
  } else if(renderUrl) {
    return (
      <RemoteContentWrapper {...wrapperProps}>
        <TruncatedText {...textProps}>{text || url}</TruncatedText>
      </RemoteContentWrapper>
    );
  } else {
    return null;
  }
}

export default React.memo(RemoteContent);
