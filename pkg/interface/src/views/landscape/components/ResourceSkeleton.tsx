import React, { ReactElement, ReactNode } from 'react';
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
  white-space: pre;
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

  return (
    <Col width="100%" height="100%" overflowY="hidden">
      <Box
        flexShrink="0"
        height='48px'
        py="2"
        px="2"
        display="flex"
        alignItems="center"
        borderBottom={1}
        borderBottomColor="washedGray"
      >
        <Box
          borderRight={1}
          borderRightColor="gray"
          pr={3}
          fontSize='1'
          mr={3}
          my="1"
          display={['block', 'none']}
          flexShrink={0}
        >
          <Link to={`/~landscape${workspace}`}><Text>{'<- Back'}</Text></Link>
        </Box>
        <Box px={1} mr={2} minWidth={0} display="flex" flexShrink={[1, 0]}>
          <Text
            mono={urbitOb.isValidPatp(title)}
            fontSize='2'
            fontWeight='700'
            display="inline-block"
            verticalAlign="middle"
            textOverflow="ellipsis"
            overflow="hidden"
            whiteSpace="pre"
            minWidth={0}
            flexShrink={1}
          >
            {title}
          </Text>
        </Box>
        <Row
          display={['none', 'flex']}
          verticalAlign="middle"
          flexShrink={2}
          minWidth={0}
          title={association?.metadata?.description}
        >
          <TruncatedText
            display={(workspace === '/messages' && (urbitOb.isValidPatp(title))) ? 'none' : 'inline-block'}
            mono={(workspace === '/messages' && !(urbitOb.isValidPatp(title)))}
            color="gray"
            minWidth={0}
            width="100%"
            mb="0"
            disableRemoteContent
          >
            {(workspace === '/messages') ? recipient : association?.metadata?.description}
          </TruncatedText>
        </Row>
        <Box flexGrow={1} flexShrink={0} />
        {canWrite && (
          <Link to={resourcePath('/new')} style={{ flexShrink: '0' }}>
            <Text bold pr='3' color='blue'>+ New Post</Text>
          </Link>
      )}
      <Link to={`${baseUrl}/settings`}>
        <Icon icon="Menu" color="gray" pr="2" />
      </Link>
      </Box>
      {children}
    </Col>
  );
}
