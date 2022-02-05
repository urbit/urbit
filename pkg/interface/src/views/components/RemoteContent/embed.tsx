import React, {
  MouseEvent,
  useCallback,
  useEffect,
  useMemo,
  useState
} from 'react';
import styled from 'styled-components';
import UnstyledEmbedContainer from 'react-oembed-container';
import {
  Box,
  BaseAnchor,
  BaseImage,
  AllSystemProps,
  allSystemStyle,
  Icon,
  Row,
  Col,
  Text
} from '@tlon/indigo-react';

import { TruncatedText } from '~/views/components/TruncatedText';
import { getModuleIcon, useHovering } from '~/logic/lib/util';
import { IconRef, PropFunc } from '~/types';
import { system } from 'styled-system';
import { Association, GraphConfig, ReferenceContent } from '@urbit/api';
import { Link } from 'react-router-dom';
import { AppPermalink, referenceToPermalink } from '~/logic/lib/permalinks';
import useMetadataState from '~/logic/state/metadata';
import { RemoteContentWrapper } from './wrapper';
import { useEmbed } from '~/logic/state/embed';
import { IS_SAFARI } from '~/logic/lib/platform';
import useDocketState, { useTreaty } from '~/logic/state/docket';
import { AppTile } from '~/views/apps/permalinks/embed';

interface RemoteContentEmbedProps {
  url: string;
  noCors?: boolean;
}

function onStopProp<T extends HTMLElement>(e: MouseEvent<T>) {
  e.stopPropagation();
}

type ImageProps = PropFunc<typeof BaseImage> & {
  objectFit?: string;
};

const Image = styled.img(system({ objectFit: true }), ...allSystemStyle);
export function RemoteContentImageEmbed(
  props: ImageProps & RemoteContentEmbedProps
) {
  const { url, ...rest } = props;
  const [noCors, setNoCors] = useState(false);
  const { hovering, bind } = useHovering();
  // maybe images aren't set up for CORS embeds
  const onError = useCallback(() => {
    setNoCors(true);
  }, []);

  return (
    <Box height="100%" width="100%" position="relative" {...bind} {...rest}>
      <BaseAnchor
        position="absolute"
        top={2}
        right={2}
        display={hovering ? 'block' : 'none'}
        target="_blank"
        rel="noopener noreferrer"
        onClick={onStopProp}
        href={url}
      >
        <Box
          backgroundColor="white"
          padding={2}
          borderRadius="50%"
          display="flex"
        >
          <Icon icon="ArrowNorthEast" />
        </Box>
      </BaseAnchor>
      <Image
        {...(noCors ? {} : { crossOrigin: 'anonymous' })}
        referrerPolicy="no-referrer"
        flexShrink={0}
        src={url}
        height="100%"
        width="100%"
        objectFit="contain"
        borderRadius={2}
        onError={onError}
        {...props}
      />
    </Box>
  );
}

type BaseAudioProps = AllSystemProps & {
  objectFit?: string;
};

const BaseAudio = styled.audio<React.PropsWithChildren<BaseAudioProps>>(
  system({ objectFit: true }),
  ...allSystemStyle
);

export function RemoteContentAudioEmbed(props: RemoteContentEmbedProps) {
  const { url, ...rest } = props;
  const [noCors, setNoCors] = useState(false);
  // maybe audio isn't set up for CORS embeds
  const onError = useCallback(() => {
    setNoCors(true);
  }, []);

  return (
    <BaseAudio
      onClick={onStopProp}
      controls
      src={url}
      objectFit="contain"
      height="24px"
      width="100%"
      minWidth={['90vw', '384px']}
      onError={onError}
      {...(noCors ? {} : { crossOrigin: 'anonymous' })}
      {...rest}
    />
  );
}

type BaseVideoProps = AllSystemProps & {
  objectFit?: string;
};

const BaseVideo = styled.video<React.PropsWithChildren<BaseVideoProps>>(
  system({ objectFit: true }),
  ...allSystemStyle
);

export function RemoteContentVideoEmbed(
  props: RemoteContentEmbedProps & PropFunc<typeof BaseVideo>
) {
  const { url, ...rest } = props;

  return (
    <BaseVideo
      onClick={onStopProp}
      controls
      src={url}
      objectFit="contain"
      height="100%"
      width="100%"
      {...rest}
    />
  );
}

const EmbedContainer = styled(UnstyledEmbedContainer)`
  width: 100%;
  grid-template-rows: 1fr max-content 1fr;
  grid-template-rows: 1fr -webkit-max-content 1fr;
  grid-template-columns: 1fr max-content 1fr;
  grid-template-columns: 1fr -webkit-max-content 1fr;
  position: relative;
  height: 100%;
  display: ${IS_SAFARI ? 'flex' : 'grid'};
  flex-direction: column;
  flex-grow: 1;
  ${IS_SAFARI ? `
    align-items: center;
    justify-content: center;
  ` : ''}
  overflow: auto;
`;

const EmbedBox = styled.div<{ aspect?: number; iHeight?: number; iWidth?: number; }>`
  display: flex;
  grid-row: 2 / 3;
  ${p => (p.aspect && !IS_SAFARI) ? `
  height: 0;
  overflow: hidden;
  padding-bottom: min(100vh - 130px, calc(100% / ${p.aspect}));
  @media screen and (max-width: ${p => p.theme.breakpoints[1]}) {
    padding-bottom: min(25vh, calc(100% / ${p.aspect}));
  }
  position: relative;
  grid-column: 1 / 4;
  ` : IS_SAFARI ? `
    width: max-content;
    height: max-content;
    max-height: 100%;
    max-width: 100%;
    flex-grow: 1;

  ` : `
    grid-column: 2 / 3;
  `}


  & iframe {
    ${p => (p.iHeight && p.iWidth) ? `
      height: ${p.iHeight}px !important;
      width: ${p.iWidth}px !important;
    ` : `
    height: 100%;
    width: 100%;
    `
    }
    ${p => p.aspect && !IS_SAFARI && 'position: absolute;'}


  }
`;

export function RemoteContentPermalinkEmbed(props: {
  reference: ReferenceContent;
}) {
  const { reference } = props;
  const permalink = referenceToPermalink(reference);

  if (permalink.type === 'graph') {
    return <RemoteContentPermalinkEmbedGraph {...permalink} />;
  } else if (permalink.type === 'group') {
    return <RemoteContentPermalinkEmbedGroup {...permalink} />;
  } else if (permalink.type === 'app') {
    return <RemoteContentPermalinkEmbedApp {...permalink} />;
  }

  return null;
}

function RemoteContentPermalinkEmbedApp({ link, ship, desk }: Omit<AppPermalink, 'type'>) {
  const treaty = useTreaty(ship, desk);

  useEffect(() => {
    if (!treaty) {
      useDocketState.getState().requestTreaty(ship, desk);
    }
  }, [treaty, ship, desk]);

  return (
    <Col
      width="100%"
      height="100%"
      padding={3}
      borderRadius={3}
      justifyContent="space-around"
      alignItems="center"
    >
      {treaty && (
        <>
          <AppTile color="washedGray" marginRight={0} {...treaty} />
          <Row><Text fontSize="1" color="gray">App: {treaty.title}</Text></Row>
        </>
      )}
    </Col>
  );
}

function RemoteContentPermalinkEmbedGroup(props: {
  group: string;
  link: string;
}) {
  const { group, link } = props;

  const association = useMetadataState(s => s.associations.groups[group]);

  const title = association?.metadata?.title ?? group.slice(6);

  return (
    <RemoteContentPermalinkEmbedBase icon="Groups" title={title} link={link} />
  );
}

function RemoteContentPermalinkEmbedGraph(props: {
  graph: string;
  link: string;
}) {
  const { graph, link } = props;

  const association = useMetadataState(
    useCallback(s => s.associations.graph[graph] as Association | null, [
      graph
    ])
  );
  const icon = association
    ? getModuleIcon((association.metadata.config as GraphConfig).graph as any)
    : 'Groups';
  const title = association?.metadata?.title ?? graph.slice(6);

  return (
    <RemoteContentPermalinkEmbedBase icon={icon} link={link} title={title} />
  );
}
function RemoteContentPermalinkEmbedBase(props: {
  icon: IconRef;
  link: string;
  title: string;
}) {
  const { icon, link, title } = props;
  return (
    <Row maxWidth="100%" overflow="hidden" gapX="2" alignItems="center" p="2">
      <Icon color="gray" icon={icon} />
      {/* @ts-ignore TS doesn't forward types here */}
      <Link to={link} component={TruncatedText} maxWidth="100%" gray>
        {title}
      </Link>
    </Row>
  );
}
type RemoteContentOembedProps = {
  renderUrl?: boolean;
  thumbnail?: boolean;
  tall?: boolean;
} & RemoteContentEmbedProps &
  PropFunc<typeof Box>;

/**
 * Some providers do not report sizes correctly, so we report an aspect ratio
 * instead of a height/width
 */
const BAD_SIZERS = [/youtu(\.?)be/];

export const RemoteContentOembed = React.forwardRef<
  HTMLDivElement,
  RemoteContentOembedProps
>((props, ref) => {
  const { url, renderUrl = false, thumbnail = false, ...rest } = props;
  const oembed = useEmbed(url);
  const embed = oembed.read();
  const fallbackError  = new Error('fallback');

  const [aspect, width, height] = useMemo(() => {
    if(!('height' in embed && typeof embed.height === 'number'
      && 'width' in embed && typeof embed.width === 'number')) {
      return [undefined, undefined, undefined];
    }
    const { height, width } = embed;
    if(BAD_SIZERS.some(r => r.test(url))) {
      return [width/height, undefined, undefined];
    }
    return [undefined, width, height];
  }, [embed, url]);

  const detail = (
    <Col
      width="100%"
      height="100%"
      justifyContent="center"
      alignItems="center"
      {...rest}
    >
      {thumbnail && embed?.['thumbnail_url'] ? (
        <BaseImage
          height="100%"
          src={embed?.['thumbnail_url']}
          style={{ objectFit: 'contain' }}
        />
      ) : !thumbnail && embed?.html ? (
        <EmbedContainer markup={embed.html}>
          <EmbedBox
            ref={ref}
            aspect={aspect}
            iHeight={height}
            iWidth={width}
            dangerouslySetInnerHTML={{ __html: embed.html }}
          ></EmbedBox>
        </EmbedContainer>
      ) : renderUrl ? (
        <RemoteContentEmbedFallback url={url} />
        ) : (() => {
 throw fallbackError;
})()
      }
    </Col>
  );
  if (!renderUrl) {
    return detail;
  }

  return (
    <RemoteContentWrapper url={url} detail={detail}>
      <TruncatedText>{embed?.title ?? url}</TruncatedText>
    </RemoteContentWrapper>
  );
});

export function RemoteContentEmbedFallback(props: RemoteContentEmbedProps) {
  const { url } = props;

  return (
    <Row maxWidth="100%" overflow="hidden" gapX="2" alignItems="center" p="2">
      <Icon color="gray" icon="ArrowExternal" />
        <BaseAnchor
          href={url}
          target="_blank"
          rel="noopener noreferrer"
          maxWidth="100%"
          overflow="hidden"
          whiteSpace="pre"
          textOverflow="ellipsis"
          color="gray"
        >
        <TruncatedText maxWidth="100%" gray>
          {url}
        </TruncatedText>
      </BaseAnchor>
    </Row>
  );
}
