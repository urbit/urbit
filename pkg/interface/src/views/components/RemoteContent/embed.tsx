import React, {
  MouseEvent,
  useState,
  useEffect,
  useRef,
  useCallback
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
  Center
} from '@tlon/indigo-react';

import { TruncatedText } from '~/views/components/TruncatedText';
import { getModuleIcon, useHovering } from '~/logic/lib/util';
import { IconRef, PropFunc } from '~/types';
import { system } from 'styled-system';
import { extract } from 'oembed-parser';
import { Association, ReferenceContent } from '@urbit/api/dist';
import { Link } from 'react-router-dom';
import { referenceToPermalink } from '~/logic/lib/permalinks';
import useMetadataState from '~/logic/state/metadata';

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
  const { url, noCors = false, ...rest } = props;
  const { hovering, bind } = useHovering();

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
  const { url, noCors, ...rest } = props;

  return (
    <BaseAudio
      onClick={onStopProp}
      controls
      src={url}
      objectFit="contain"
      height="100%"
      width="100%"
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
  const { url, noCors, ...rest } = props;

  return (
    <BaseVideo
      onClick={onStopProp}
      controls
      src={url}
      objectFit="contain"
      height="100%"
      width="100%"
      {...(noCors ? {} : { crossOrigin: 'anonymous' })}
      {...rest}
    />
  );
}

const EmbedContainer = styled(UnstyledEmbedContainer)`
  width: 100%;
  height: 100%;
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
  }

  return null;
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
    ? getModuleIcon(association.metadata.config.graph)
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

export function RemoteContentOembed(
  props: {
    renderUrl?: boolean;
    thumbnail?: boolean;
  } & RemoteContentEmbedProps &
    PropFunc<typeof Box>
) {
  const ref = useRef<HTMLDivElement>();
  const { url, renderUrl = false, thumbnail = false, ...rest } = props;
  const [embed, setEmbed] = useState<any>();

  useEffect(() => {
    const getEmbed = async () => {
      try {
        const { width, height } = ref.current.getBoundingClientRect();

        const oembed = await extract(url, {
          maxheight: Math.floor(height),
          maxwidth: Math.floor(width)
        });
        setEmbed(oembed);
      } catch (e) {
        console.error(e);
      }
    };

    getEmbed();
  }, [url]);

  return (
    <Col
      ref={ref}
      mb={2}
      width="100%"
      flexShrink={0}
      height="100%"
      justifyContent="center"
      alignItems="center"
      {...rest}
    >
      <TruncatedText
        display={
          renderUrl && embed?.title && embed.title !== url
            ? 'inline-block'
            : 'none'
        }
        fontWeight="bold"
        width="100%"
      >
        {embed?.title}
      </TruncatedText>
      {thumbnail && embed?.['thumbnail_url'] ? (
        <BaseImage
          height="100%"
          src={embed?.['thumbnail_url']}
          style={{ objectFit: 'contain' }}
        />
      ) : !thumbnail && embed?.html ? (
        <EmbedContainer markup={embed.html}>
          <Center
            height="100%"
            width="100%"
            dangerouslySetInnerHTML={{ __html: embed.html }}
          ></Center>
        </EmbedContainer>
      ) : (
        <RemoteContentEmbedFallback url={url} />
      )}
    </Col>
  );
}

export function RemoteContentEmbedFallback(props: RemoteContentEmbedProps) {
  const { url } = props;

  return (
    <Row maxWidth="100%" overflow="hidden" gapX="2" alignItems="center" p="2">
      <Icon color="gray" icon="ArrowExternal" />
      <TruncatedText maxWidth="100%" gray>
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
          {url}
        </BaseAnchor>
      </TruncatedText>
    </Row>
  );
}
