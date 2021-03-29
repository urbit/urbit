import React, { ReactElement, ReactNode, useRef, useState } from 'react';
import { Icon, Box, Col, Row, Text } from '@tlon/indigo-react';
import styled from 'styled-components';
import { Link } from 'react-router-dom';
import urbitOb from 'urbit-ob';

import { Association } from '@urbit/api/metadata';
import { Groups, Rolodex } from '@urbit/api';

import RichText from '~/views/components/RichText';
import GlobalApi from '~/logic/api/global';
import { isWriter } from '~/logic/lib/group';
import { getItemTitle } from '~/logic/lib/util';
import useContactState from '~/logic/state/contact';
import useGroupState from '~/logic/state/group';

const TruncatedText = styled(RichText)`
  white-space: nowrap;
  text-overflow: ellipsis;
  overflow: hidden;
`;

type ResourceSkeletonProps = {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
  children: ReactNode;
  title?: string;
  groupTags?: any;
};

export function ResourceSkeleton(props: ResourceSkeletonProps): ReactElement {
  const { association, baseUrl, children } = props;
  const app = association?.metadata?.module || association['app-name'];
  const rid = association.resource;
  const groups = useGroupState(state => state.groups);
  const group = groups[association.group];
  let workspace = association.group;
  const actionsRef = useRef(null);

  if (group?.hidden && app === 'chat') {
    workspace = '/messages';
  } else if (group?.hidden) {
    workspace = '/home';
  }

  let title = (workspace === '/messages')
    ? getItemTitle(association)
    : association?.metadata?.title;

  let recipient = "";

  const contacts = useContactState(state => state.contacts);

  if (urbitOb.isValidPatp(title)) {
    recipient = title;
    title = (contacts?.[title]?.nickname) ? contacts[title].nickname : title;
  } else {
    recipient = Array.from(group ? group.members : []).map(e => `~${e}`).join(", ")
  }

  const [, , ship, resource] = rid.split('/');

  const resourcePath = (p: string) => baseUrl + p;

  const isOwn = `~${window.ship}` === ship;
  let canWrite = (app === 'publish') ? true : false;

  if (!isWriter(group, association.resource)) {
    canWrite = isOwn;
  }

  const BackLink = () => (
    <Box
      borderRight={1}
      borderRightColor='gray'
      pr={3}
      fontSize='1'
      mr='12px'
      my='1'
      flexShrink='0'
      display={['block','none']}
    >
      <Link to={`/~landscape${workspace}`}>
        <Text>{'<- Back'}</Text>
      </Link>
    </Box>
  );

  const Title = () => (
    <Text
      mono={urbitOb.isValidPatp(title)}
      fontSize='2'
      fontWeight='700'
      textOverflow='ellipsis'
      overflow='hidden'
      whiteSpace='nowrap'
      minWidth='0'
      maxWidth={association?.metadata?.description ? ['100%', '50%'] : 'none'}
      mr='2'
      ml='1'
      flexShrink={['1', '0']}
    >
      {title}
    </Text>
  );

  const Description = () => (
    <TruncatedText
      display={['none','inline']}
      mono={workspace === '/messages' && !urbitOb.isValidPatp(title)}
      color='gray'
      mb='0'
      minWidth='0'
      maxWidth='50%'
      flexShrink='1'
      disableRemoteContent
    >
      {workspace === '/messages'
        ? recipient
        : association?.metadata?.description}
    </TruncatedText>
  );

  const WriterControls = () => (
    <Link to={resourcePath('/new')}>
      <Text bold pr='3' color='blue'>
        + New Post
      </Text>
    </Link>
  );

  const MenuControl = () => (
    <Link to={`${baseUrl}/settings`}>
      <Icon icon='Menu' color='gray' pr='2' />
    </Link>
  );

  const actRef = actionsRef.current;
  let actionsWidth = 0;

  if (actRef) {
    actionsWidth = actRef.clientWidth;
  }
  

  return (
    <Col width='100%' height='100%' overflow='hidden'>
      <Box
        flexShrink='0'
        height='48px'
        py='2'
        px='2'
        borderBottom={1}
        borderBottomColor='washedGray'
        display='flex'
        justifyContent='space-between'
        alignItems='center'
      >
        <Box
          display='flex'
          alignItems='baseline'
          width={`calc(100% - ${actionsWidth}px - 16px)`}
          flexShrink='0'
        >
          <BackLink />
          <Title />
          <Description />
        </Box>
        <Box
          ml={3}
          display='flex'
          alignItems='center'
          flexShrink='0'
          ref={actionsRef}
        >
          {canWrite && <WriterControls />}
          <MenuControl />
        </Box>
      </Box>
      {children}
    </Col>
  );
}
