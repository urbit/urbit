import { hasProvider } from 'oembed-parser';
import React from 'react';
import useSettingsState from '~/logic/state/settings';
import {
  RemoteContentAudioEmbed,
  RemoteContentImageEmbed,
  RemoteContentOembed,
  RemoteContentVideoEmbed
} from './embed';
import { TruncatedText } from '~/views/components/TruncatedText';
import { RemoteContentWrapper } from './wrapper';

export interface RemoteContentProps {
  /**
   * Url to render
   */
  url: string;
  /**
   * Should render the URL as part of the display of the RemoteContent.
   *
   * If false, then only the embedded content, if any will be rendered
   */
  renderUrl?: boolean;
  /**
   * A ref to the div that contains an iframe
   */
  embedRef?: (el: HTMLDivElement | null) => void;
  /**
   * Is inside transclusion
   */
  transcluded?: any;
  /**
   * Render in a tall formatting context, e.g. images will take the full width
   * of their containers etc.
   */
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
    transcluded,
    tall = false,
    renderUrl = true
  } = props;

  const remoteContentPolicy = useSettingsState(s => s.remoteContentPolicy);
  const isImage = IMAGE_REGEX.test(url);
  const isAudio = AUDIO_REGEX.test(url);
  const isVideo = VIDEO_REGEX.test(url);
  const isOembed = hasProvider(url);
  const wrapperProps = {
    url,
    tall,
    embedOnly: !renderUrl || tall
  };

  if (isImage && remoteContentPolicy.imageShown) {
    return (
      <RemoteContentWrapper {...wrapperProps} noOp={transcluded} replaced>
        <RemoteContentImageEmbed url={url} />
      </RemoteContentWrapper>
    );
  } else if (isAudio && remoteContentPolicy.audioShown) {
    return (
      <RemoteContentWrapper {...wrapperProps}>
        <RemoteContentAudioEmbed url={url} />
      </RemoteContentWrapper>
    );
  } else if (isVideo && remoteContentPolicy.videoShown) {
    return (
      <RemoteContentWrapper
        {...wrapperProps}
        detail={<RemoteContentVideoEmbed url={url} />}
      >
        <TruncatedText>{url}</TruncatedText>
      </RemoteContentWrapper>
    );
  } else if (isOembed && remoteContentPolicy.oembedShown) {
    return (
      <RemoteContentOembed ref={embedRef} url={url} renderUrl={renderUrl} />
    );
  } else if (renderUrl) {
    return (
      <RemoteContentWrapper {...wrapperProps}>
        <TruncatedText>{url}</TruncatedText>
      </RemoteContentWrapper>
    );
  } else {
    return null;
  }
}

export default React.memo(RemoteContent);
