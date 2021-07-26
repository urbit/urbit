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
import { IMAGE_REGEX, AUDIO_REGEX, VIDEO_REGEX } from '~/logic/lib/util';
import AsyncFallback from '../AsyncFallback';

export interface RemoteContentProps {
  /**
   * Url to render
   */
  url: string;
  /**
   * The title for the url, or a string describing it
   *
   */
  title: string;
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

const emptyRef = () => {};
export function RemoteContent(props: RemoteContentProps) {
  const {
    url,
    embedRef = emptyRef,
    transcluded,
    title,
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

  const fallback = !renderUrl ? null : (
    <RemoteContentWrapper {...wrapperProps}>
      <TruncatedText>{url}</TruncatedText>
    </RemoteContentWrapper>
  );

  if (isImage && remoteContentPolicy.imageShown) {
    return (
      <RemoteContentWrapper {...wrapperProps} noOp={transcluded} replaced>
        <RemoteContentImageEmbed title={title} url={url} />
      </RemoteContentWrapper>
    );
  } else if (isAudio && remoteContentPolicy.audioShown) {
    return (
      <RemoteContentWrapper {...wrapperProps}>
        <RemoteContentAudioEmbed title={title} url={url} />
      </RemoteContentWrapper>
    );
  } else if (isVideo && remoteContentPolicy.videoShown) {
    return (
      <RemoteContentWrapper
        {...wrapperProps}
        detail={<RemoteContentVideoEmbed title={title} url={url} />}
      >
        <TruncatedText>{url}</TruncatedText>
      </RemoteContentWrapper>
    );
  } else if (isOembed && remoteContentPolicy.oembedShown) {
    return (
      <AsyncFallback fallback={fallback}>
        <RemoteContentOembed ref={embedRef} url={url} renderUrl={renderUrl} />
      </AsyncFallback>
    );
  }
  return fallback;
}

export default React.memo(RemoteContent);
