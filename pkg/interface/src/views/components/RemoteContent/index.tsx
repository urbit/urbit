import {
  Box
} from '@tlon/indigo-react';
import React from 'react';
import useSettingsState from '~/logic/state/settings';
import {
  RemoteContentAudioEmbed,
  RemoteContentImageEmbed,
  RemoteContentOembed,
  RemoteContentVideoEmbed,
  RemoteContentEmbedFallback
} from './embed';
import { useEmbed } from '~/logic/state/embed';
import { Suspender } from '~/logic/lib/suspend';
import { TruncatedText } from '~/views/components/TruncatedText';
import { RemoteContentWrapper } from './wrapper';
import AsyncFallback from '../AsyncFallback';

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
  /(\.jpg|\.img|\.png|\.gif|\.tiff|\.jpeg|\.webp|\.webm|\.svg)$/i
);
export const AUDIO_REGEX = new RegExp(/(\.mp3|\.wav|\.ogg|\.m4a)$/i);
export const VIDEO_REGEX = new RegExp(/(\.mov|\.mp4|\.ogv)$/i);

// This is used to prevent our oembed parser from 
// trying to embed facebook/instagram links, which require an API key
const isFacebookGraphDependent = (url: string) => {
  const caseDesensitizedURL = url.toLowerCase()
  return (caseDesensitizedURL.includes('facebook.com') || caseDesensitizedURL.includes('instagram.com'))
}

export const validOembedCheck = (embed: Suspender<any>, url: string) => {
  if (!isFacebookGraphDependent(url)) {
    if (!embed.read().hasOwnProperty("error")) {
      return true
    }
  }
  return false
}

export const RemoteContent = (props: RemoteContentProps) => {
  const {url, ...rest} = props
  return(
    <AsyncFallback fallback={<RemoteContentEmbedFallback url={url} />}>
          <RemoteContentInner url={url} {...rest}/>
    </AsyncFallback>
  )
}


const emptyRef = () => {};
function RemoteContentInner(props: RemoteContentProps) {
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
  const oembed = useEmbed(url);
  const isOembed = validOembedCheck(oembed, url);

  const wrapperProps = {
    url,
    tall,
    embedOnly: !renderUrl || tall
  };

  const fallback = null;

  if (isImage && remoteContentPolicy.imageShown) {
    return (
      <Box mt={1} mb={2} flexShrink={0}>
        <RemoteContentWrapper {...wrapperProps} noOp={transcluded} replaced>
          <RemoteContentImageEmbed url={url} stretch={tall} />
        </RemoteContentWrapper>
      </Box>
    );
  } else if (isAudio && remoteContentPolicy.audioShown) {
    return (
      <Box mt={1} mb={2} flexShrink={0}>
        <RemoteContentWrapper {...wrapperProps}>
          <RemoteContentAudioEmbed url={url} />
        </RemoteContentWrapper>
      </Box>
    );
  } else if (isVideo && remoteContentPolicy.videoShown) {
    return (
      <Box mt={1} mb={2} flexShrink={0}>
        <RemoteContentWrapper
          {...wrapperProps}
          detail={<RemoteContentVideoEmbed url={url} />}
        >
          <TruncatedText>{url}</TruncatedText>
        </RemoteContentWrapper>
      </Box>
    );
  } else if (isOembed && remoteContentPolicy.oembedShown) {
    return (
      <Box mt={1} mb={2} flexShrink={0}>
        <AsyncFallback fallback={fallback}>
          <RemoteContentOembed ref={embedRef} url={url} renderUrl={renderUrl} oembed={oembed} />
        </AsyncFallback>
      </Box>
    );
  }
  return fallback;
}

export default React.memo(RemoteContent);
