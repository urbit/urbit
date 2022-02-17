import _ from 'lodash';
import { Box, Col, Icon, Row, Text } from '@tlon/indigo-react';
import { AppName, Association } from '@urbit/api';
import React, { ReactElement, ReactNode, useCallback, useMemo, useState } from 'react';
import { Link } from 'react-router-dom';
import styled from 'styled-components';
import urbitOb from 'urbit-ob';
import { isWriter } from '~/logic/lib/group';
import { useResize } from '~/logic/lib/useResize';
import { getItemTitle } from '~/logic/lib/util';
import useContactState from '~/logic/state/contact';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import useGroupState from '~/logic/state/group';
import { Dropdown } from '~/views/components/Dropdown';
import RichText from '~/views/components/RichText';
import { MessageInvite } from '~/views/landscape/components/MessageInvite';
import { IS_MOBILE } from '~/logic/lib/platform';
import useMetadataState from '~/logic/state/metadata';
import { getPermalinkForGraph } from '~/logic/lib/permalinks';
import { useCopy } from '~/logic/lib/useCopy';

const TruncatedText = styled(RichText)`
  white-space: nowrap;
  text-overflow: ellipsis;
  overflow: hidden;
  display: none;
  @media screen and (min-width: ${p => p.theme.breakpoints[0]}) {
    display: inline;
  }
`;

const participantNames = (str: string, contacts, hideNicknames) => {
  if (_.includes(str, ',') && _.startsWith(str, '~')) {
    const names = _.split(str, ', ');
    return names.map((name, idx) => {
      if (urbitOb.isValidPatp(name)) {
        if (contacts[name]?.nickname && !hideNicknames)
          return (
            <Text key={name} fontSize={2} fontWeight='600'>
              {contacts[name]?.nickname}
              {idx + 1 != names.length ? ', ' : null}
            </Text>
          );
        return (
          <Text key={name} mono fontSize={2} fontWeight='600'>
            {name}
            <Text fontSize={2} fontWeight='600'>
              {idx + 1 != names.length ? ', ' : null}
            </Text>
          </Text>
        );
      } else {
        return name;
      }
    });
  } else {
    return str;
  }
};

type ResourceSkeletonProps = {
  association: Association;
  baseUrl: string;
  children: ReactNode;
  title?: string;
  groupTags?: any;
};

export function ResourceSkeleton(props: ResourceSkeletonProps): ReactElement {
  const { association, baseUrl, children } = props;
  let app: string = association['app-name'];
  if (association?.metadata?.config && 'graph' in association.metadata.config) {
    app = association.metadata.config.graph as AppName;
  }
  const rid = association.resource;
  const { groups } = useGroupState();
  const { associations } = useMetadataState();
  const { hideNicknames } = useSettingsState(selectCalmState);
  const group = groups[association.group];
  let workspace = association.group;
  const [actionsWidth, setActionsWidth] = useState(0);
  const permalink = useMemo(() => getPermalinkForGraph(association.group, association.resource), [association]);
  const { doCopy, copyDisplay } = useCopy(permalink, <Icon icon='Copy' color='gray' pr={2} />, <Icon icon='Checkmark' color='gray' pr={2} />);

  if (group?.hidden && app === 'chat') {
    workspace = '/messages';
  } else if (group?.hidden) {
    workspace = '/home';
  }

  let title = (workspace === '/messages')
    ? getItemTitle(association)
    : association?.metadata?.title;

  let recipient = '';

  const contacts = useContactState(state => state.contacts);

  if (urbitOb.isValidPatp(title) && !hideNicknames) {
    recipient = title;
    title = (contacts?.[title]?.nickname) ? contacts[title].nickname : title;
  } else {
    recipient = Array.from(group ? group.members : []).map(e => `~${e}`).join(', ');
  }

  const [, , ship, resource] = rid.split('/');

  const resourcePath = (p: string) => baseUrl + p;

  const isOwn = `~${window.ship}` === ship;
  let canWrite = (app === 'publish') ? true : false;

  if (!isWriter(group, association.resource)) {
    canWrite = isOwn;
  }

  const backLink = (
    <Box
      borderRight={1}
      borderRightColor='gray'
      pr={3}
      fontSize={1}
      mr='12px'
      my={1}
      flexShrink={0}
      display={['block','none']}
    >
      <Link to={`/~landscape${workspace}`}>
        <Text>{'<- Back'}</Text>
      </Link>
    </Box>
  );

  // TODO: if a resource of a group, show breadcrumbs on mobile
  const titleText = IS_MOBILE && workspace === association.group
  ? (
    <Row width="100%">
      <Text maxWidth="38%" textOverflow='ellipsis' overflow='hidden' whiteSpace='nowrap' fontWeight={600} fontSize="14px">{associations.groups[workspace]?.metadata?.title}</Text>
      <Text fontWeight={600} fontSize="14px" mx={1}>{'>'}</Text>
      <Text maxWidth="38%" textOverflow='ellipsis' overflow='hidden' whiteSpace='nowrap' fontWeight={600} fontSize="14px">{title}</Text>
    </Row>
  ) : (
    <Text
      mono={urbitOb.isValidPatp(title)}
      fontSize={2}
      fontWeight='600'
      textOverflow='ellipsis'
      overflow='hidden'
      whiteSpace='nowrap'
      minWidth={0}
      maxWidth={association?.metadata?.description ? ['100%', '50%'] : 'none'}
      mr='2'
      ml='1'
      flexShrink={1}
    >
      {workspace === '/messages' && !urbitOb.isValidPatp(title)
        ? participantNames(title, contacts, hideNicknames)
        : title}
    </Text>
  );

  const description = (
    <TruncatedText
      display={['none','inline']}
      mono={workspace === '/messages' && !association?.metadata?.description}
      color='gray'
      mb={0}
      minWidth={0}
      maxWidth='50%'
      flexShrink={1}
      disableRemoteContent
    >
      {workspace === '/messages' && !association?.metadata?.description
        ? recipient
        : association?.metadata?.description}
    </TruncatedText>
  );

  const extraControls =
    (workspace === '/messages' && isOwn && !resource.startsWith('dm-')) ?  (
        <Dropdown
          flexShrink={0}
          dropWidth='300px'
          width='auto'
          alignY='top'
          alignX='right'
          options={
            <Col
              backgroundColor='white'
              border={1}
              borderRadius={2}
              borderColor='lightGray'
              color='washedGray'
              boxShadow='0px 0px 0px 3px'
            >
              <MessageInvite association={association} />
            </Col>
          }
        >
          <Text bold pr='3' color='blue'>
            + Add Ship
          </Text>
        </Dropdown>
      ) : canWrite ? (
        <Link to={resourcePath('/new')}>
          <Text bold pr='3' color='blue'>
            + New Post
          </Text>
        </Link>
      ) : null;

  const copyControl = (
    <Box onClick={doCopy} cursor="pointer">
      {copyDisplay}
    </Box>
  );

  const menuControl = (
    <Link to={`${baseUrl}/settings`}>
      <Icon icon='Menu' color='gray' pr={2} />
    </Link>
  );

  const bind = useResize<HTMLDivElement>(useCallback((entry) => {
    setActionsWidth(entry.borderBoxSize[0].inlineSize);
  }, []));

  return (
    <Col width='100%' height='100%' overflow='hidden'>
      <Box
        flexShrink={0}
        height='48px'
        py={2}
        px={2}
        borderBottom={1}
        borderBottomColor='lightGray'
        display='flex'
        justifyContent='space-between'
        alignItems='center'
      >
        <Box
          display='flex'
          alignItems='baseline'
          width={`calc(100% - ${actionsWidth}px - 16px)`}
          flexShrink={1}
          minWidth={0}
        >
          {backLink}
          {titleText}
          {description}
        </Box>
        <Box
          ml={3}
          display='flex'
          alignItems='center'
          flexShrink={0}
          {...bind}
        >
          {extraControls}
          {copyControl}
          {menuControl}
        </Box>
      </Box>
      {children}
    </Col>
  );
}
