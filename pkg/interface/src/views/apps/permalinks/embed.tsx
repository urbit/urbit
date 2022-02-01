import { BaseAnchor, Box, BoxProps, Button, Center, Col, H3, Icon, Image, Row, Text } from '@tlon/indigo-react';
import { Association, GraphNode, resourceFromPath, GraphConfig, Treaty, deSig } from '@urbit/api';
import React, { useCallback, useEffect, useState } from 'react';
import _ from 'lodash';
import { Link, useLocation } from 'react-router-dom';
import {
  getPermalinkForGraph, GraphPermalink as IGraphPermalink, parsePermalink,
  AppPermalink as IAppPermalink
} from '~/logic/lib/permalinks';
import { getModuleIcon, GraphModule } from '~/logic/lib/util';
import { useVirtualResizeProp } from '~/logic/lib/virtualContext';
import useGraphState  from '~/logic/state/graph';
import useMetadataState from '~/logic/state/metadata';
import { GroupLink } from '~/views/components/GroupLink';
import { TranscludedNode } from './TranscludedNode';
import styled from 'styled-components';
import Author from '~/views/components/Author';
import useDocketState, { useTreaty } from '~/logic/state/docket';
import { createJoinParams } from '~/views/landscape/components/Join/Join';

function Placeholder(type) {
  const lines = (type) => {
    switch (type) {
      case 'publish':
        return 5;
      case 'post':
        return 3;
      default:
        return 1;
    }
  };
  return (
    <Box p='12px 12px 6px'>
      <Row mb='6px' height="4">
        <Box
          backgroundColor="washedGray"
          size="4"
          marginRight="2"
          borderRadius="2"
        />
        <Box
          backgroundColor="washedGray"
          height="4"
          width="25%"
          borderRadius="2"
        />
      </Row>
      {_.times(lines(type), i => (
        <Row margin="6px" ml='32px' height="4">
          <Box
            backgroundColor="washedGray"
            height="4"
            width="100%"
            borderRadius="2"
          />
        </Row>
      ))}
    </Box>
  );
}

function GroupPermalink(props: { group: string; }) {
  const { group } = props;
  return (
    <GroupLink
      resource={group}
      pl={2}
      border={1}
      borderRadius={2}
      borderColor="lightGray"
    />
  );
}

function GraphPermalink(
  props: IGraphPermalink & {
    transcluded: number;
    pending?: boolean;
    showOurContact?: boolean;
    full?: boolean;
  }
) {
  const { full = false, showOurContact, pending, graph, group, index, transcluded } = props;
  const location = useLocation();
  const { ship, name } = resourceFromPath(graph);
  const node = useGraphState(
    useCallback(s => s.looseNodes?.[`${deSig(ship)}/${name}`]?.[index] as GraphNode, [
      graph,
      index
    ])
  );
  const [errored, setErrored] = useState(false);
  const [loading, setLoading] = useState(false);
  const getNode = useGraphState(s => s.getNode);
  const association = useMetadataState(
    useCallback(s => s.associations.graph[graph] as Association | null, [
      graph
    ])
  );

  useVirtualResizeProp(Boolean(node));
  useEffect(() => {
    (async () => {
      if (pending || !index) {
        return;
      }
      try {
        setLoading(true);
        await getNode(ship, name, index);
        setLoading(false);
      } catch (e) {
        console.log(e);
        setLoading(false);
        setErrored(true);
      }
    })();
  }, [pending, graph, index]);
  const showTransclusion = Boolean(association && node && transcluded < 1);
  const permalink = (() => {
    const link = `/perma${getPermalinkForGraph(group, graph, index).slice(16)}`;
    return (!association && !loading)
      ? { search: createJoinParams('groups', group, link)  } :  link;
  })();

  const [nodeGroupHost, nodeGroupName] = association?.group.split('/').slice(-2) ?? ['Unknown', 'Unknown'];
  const [nodeChannelHost, nodeChannelName] = association?.resource
    .split('/')
    .slice(-2) ?? ['Unknown', 'Unknown'];
  const [
    locChannelName,
    locChannelHost,
    ,
    ,
    ,
    locGroupName,
    locGroupHost
  ] = location.pathname.split('/').reverse();

  const isInSameResource =
    locChannelHost === nodeChannelHost &&
    locChannelName === nodeChannelName &&
    locGroupName === nodeGroupName &&
    locGroupHost === nodeGroupHost;

  return (
    <Col
      as={Link}
      to={permalink}
      width="100%"
      bg="white"
      maxWidth={full ? null : '500px'}
      border={full ? null : '1'}
      borderColor="lightGray"
      color="black"
      borderRadius={2}
      onClick={(e) => {
        e.stopPropagation();
      }}
    >
      {loading && association && !errored && Placeholder((association.metadata.config as GraphConfig).graph)}
      {showTransclusion && index && !loading && (
        <TranscludedNode
          transcluded={transcluded + 1}
          node={node}
          assoc={association!}
          showOurContact={showOurContact}
        />
      )}
      {association && !isInSameResource && !loading && (
        <PermalinkDetails
          known
          showTransclusion={showTransclusion}
          icon={getModuleIcon((association.metadata.config as GraphConfig).graph as GraphModule)}
          title={association.metadata.title}
        />
      )}
      {association && isInSameResource && transcluded === 2 && !loading && (
        <PermalinkDetails
          known
          showTransclusion={showTransclusion}
          icon={getModuleIcon((association.metadata.config as GraphConfig).graph as GraphModule)}
          title={association.metadata.title}
        />
      )}
      {isInSameResource && transcluded !== 2 && !loading && <Row height='2' />}
      {!association && !loading && (
        <PermalinkDetails
          icon="Groups"
          showDetails={false}
          title={graph.slice(5)}
        />
      )}
    </Col>
  );
}

const ClampedText = styled(Text)`
  display: -webkit-box;
  line-clamp: 2;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
`;

type AppTileProps = Treaty & BoxProps;

export function AppTile({ color, image, ...props }: AppTileProps) {
  const [imageError, setImageError] = useState(false);

  return (
    <Box
      position="relative"
      flex="none"
      height={['48px', '132px']}
      width={['48px', '132px']}
      marginRight={3}
      borderRadius={3}
      bg={color || 'washedGray'}
      {...props}
    >
      {image && !imageError && (
        <Image
          src={image}
          position="absolute"
          top="0"
          left="0"
          width="100%"
          height="100%"
          onError={() => setImageError(true)}
        />
      )}
    </Box>
  );
}

const AppSkeleton = props => (
  <Box
    width="100%"
    height="14px"
    bg="washedGray"
    borderRadius={2}
    marginBottom={2}
    {...props}
  />
);

function AppPermalink({ link, ship, desk }: Omit<IAppPermalink, 'type'>) {
  const treaty = useTreaty(ship, desk);
  const hasProtocolHandling = Boolean(window?.navigator?.registerProtocolHandler);
  const href = hasProtocolHandling ? link : `/apps/grid/perma?ext=${link}`;

  useEffect(() => {
    if (!treaty) {
      useDocketState.getState().requestTreaty(ship, desk);
    }
  }, [treaty, ship, desk]);

  return (
    <Row
      display="inline-flex"
      width="100%"
      maxWidth="500px"
      padding={3}
      bg="washedGray"
      borderRadius={3}
    >
      <AppTile display={['none', 'block']} {...treaty} />
      <Col flex="1">
        <Row flexDirection={['row', 'column']} alignItems={['center', 'start']} marginBottom={2}>
          <AppTile display={['block', 'none']} {...treaty} />
          <Col>
            <H3 color="black">{ treaty?.title || '%' + desk }</H3>
            <Author ship={treaty?.ship || ship} showImage dontShowTime={true} />
          </Col>
        </Row>
        {treaty && <ClampedText marginBottom={2} color="gray">{treaty.info}</ClampedText>}
        {!treaty && (
          <>
            <AppSkeleton />
            <AppSkeleton width="80%" />
          </>
        )}
        <Button
          as="a"
          href={href}
          primary
          alignSelf="start"
          display="inline-flex"
          marginTop="auto"
          target="_blank"
          rel="noreferrer"
        >
          Open App
        </Button>
      </Col>
    </Row>
  );
}

function PermalinkDetails(props: {
  title: string;
  icon: any;
  showTransclusion?: boolean;
  showDetails?: boolean;
  known?: boolean;
}) {
  const { title, icon, known, showTransclusion } = props;
  const rowTransclusionStyle = showTransclusion
    ? { p: '12px 12px 11px 11px' }
    : { p: '12px' };

  return (
    <Row
      {...rowTransclusionStyle}
      alignItems="center"
      justifyContent="space-between"
      width="100%"
    >
      <Row gapX="2" alignItems="center">
        <Box width={4} height={4}>
          <Center width={4} height={4}>
            <Icon icon={icon} color='gray' />
          </Center>
        </Box>
        <Text gray mono={!known}>
          {title}
        </Text>
      </Row>
    </Row>
  );
}

export function PermalinkEmbed(props: {
  link: string;
  association?: Association;
  transcluded: number;
  showOurContact?: boolean;
  full?: boolean;
  pending?: any;
}) {
  const permalink = parsePermalink(props.link);

  if (!permalink) {
    return <BaseAnchor href={props.link}>{props.link}</BaseAnchor>;
  }

  switch (permalink.type) {
    case 'group':
      return <GroupPermalink group={permalink.group} />;
    case 'graph':
      return (
        <GraphPermalink
          transcluded={props.transcluded}
          {...permalink}
          full={props.full}
          showOurContact={props.showOurContact}
        />
      );
    case 'app':
      return (
        <AppPermalink {...permalink} />
      );
  }
}
