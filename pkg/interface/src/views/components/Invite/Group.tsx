import { css } from '@styled-system/css';
import {
  Box,
  Icon,

  LoadingSpinner, Row, Text
} from '@tlon/indigo-react';
import {
  Invite, joinProgress,

  JoinRequest,

  Metadata, MetadataUpdatePreview,

  resourceFromPath
} from '@urbit/api';
import _ from 'lodash';
import React, { ReactElement, ReactNode, useCallback } from 'react';
import { useHistory } from 'react-router-dom';
import styled from 'styled-components';
import GlobalApi from '~/logic/api/global';
import { useRunIO } from '~/logic/lib/useRunIO';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { cite, isDm } from '~/logic/lib/util';
import useGraphState from '~/logic/state/graph';
import useGroupState from '~/logic/state/group';
import useMetadataState, { useAssocForGraph } from '~/logic/state/metadata';
import { PropFunc } from '~/types';
import { Header } from '~/views/apps/notifications/header';
import { NotificationWrapper } from '~/views/apps/notifications/notification';
import { MetadataIcon } from '~/views/landscape/components/MetadataIcon';
import { StatelessAsyncButton } from '../StatelessAsyncButton';

interface GroupInviteProps {
  preview?: MetadataUpdatePreview;
  status?: JoinRequest;
  app?: string;
  uid?: string;
  invite?: Invite;
  resource: string;
  api: GlobalApi;
}

function Elbow(
  props: { size?: number; color?: string } & PropFunc<typeof Box>
) {
  const { size = 12, color = 'lightGray', ...rest } = props;

  return (
    <Box
      {...rest}
      overflow="hidden"
      width={size}
      height={size}
      position="relative"
    >
      <Box
        border="2px solid"
        borderRadius={3}
        borderColor={color}
        position="absolute"
        left="0px"
        bottom="0px"
        width={size * 2}
        height={size * 2}
      />
    </Box>
  );
}

const description: string[] = [
  'Contacting host...',
  'Retrieving data...',
  'Finished join',
  'Unable to join, you do not have the correct permissions',
  'Internal error, please file an issue'
];

function inviteUrl(hidden: boolean, resource: string, metadata?: Metadata) {
  if (!hidden) {
    return `/~landscape${resource}`;
  }

  if (metadata?.config.graph === 'chat') {
    return `/~landscape/messages/resource/${metadata?.config?.graph}${resource}`;
  } else {
    return `/~landscape/home/resource/${metadata?.config?.graph}${resource}`;
  }
}
function InviteMetadata(props: {
  preview?: MetadataUpdatePreview;
  resource: string;
}) {
  const { resource, preview } = props;
  const { ship, name } = resourceFromPath(resource);
  const dm = isDm(resource);
  if (dm) {
    return null;
  }

  const container = (children: ReactNode) => (
    <Row overflow="hidden" height="4" gapX="2" alignItems="center">
      {children}
    </Row>
  );

  if (preview) {
    const { title } = preview.metadata;
    const { members } = preview;
    return container(
      <>
        <MetadataIcon height={4} width={4} metadata={preview.metadata} />
        <Text fontWeight="medium">{title}</Text>
        <Text gray fontWeight="medium">
          {members} Member{members > 1 ? 's' : ''}
        </Text>
      </>
    );
  }

  return container(
    <>
      <Text whiteSpace="nowrap" textOverflow="ellipsis" ml="1px" mb="2px" mono>
        {cite(ship)}/{name}
      </Text>
    </>
  );
}

function InviteStatus(props: { status?: JoinRequest }) {
  const { status } = props;

  if (!status) {
    return null;
  }

  const current = status && joinProgress.indexOf(status.progress);
  const desc = _.isNumber(current) && description[current];
  return (
    <Row gapX="1" alignItems="center" height={4}>
      { status.progress === 'done' ? <Icon icon="Checkmark" /> : <LoadingSpinner dark /> }
      <Text gray>{desc}</Text>
    </Row>
  );
}

export function useInviteAccept(
  resource: string,
  api: GlobalApi,
  app?: string,
  uid?: string
) {
  const { ship, name } = resourceFromPath(resource);
  const history = useHistory();
  const associations = useMetadataState(s => s.associations);
  const groups = useGroupState(s => s.groups);
  const graphKeys = useGraphState(s => s.graphKeys);

  const waiter = useWaitForProps({ associations, graphKeys, groups });
  return useRunIO<void, boolean>(
    async () => {
      if (!(app && uid)) {
        return false;
      }
      if (resource in groups) {
        await api.invite.decline(app, uid);
        return false;
      }

      await api.groups.join(ship, name);
      await api.invite.accept(app, uid);
      await waiter((p) => {
        return (
          (resource in p.groups &&
            resource in (p.associations?.graph ?? {}) &&
            p.graphKeys.has(resource.slice(7))) ||
          resource in (p.associations?.groups ?? {})
        );
      });
      return true;
    },
    (success: boolean) => {
      if (!success) {
        return;
      }
      const redir = inviteUrl(
        groups?.[resource]?.hidden,
        resource,
        associations?.graph?.[resource]?.metadata
      );
      if (redir) {
        // weird race condition
        setTimeout(() => {
          history.push(redir);
        }, 200);
      }
    },
    resource
  );
}

function InviteActions(props: {
  status?: JoinRequest;
  resource: string;
  api: GlobalApi;
  app?: string;
  uid?: string;
}) {
  const { resource, api, app, uid } = props;
  const inviteAccept = useInviteAccept(resource, api, app, uid);

  const inviteDecline = useCallback(async () => {
    if (!(app && uid)) {
      return;
    }
    await api.invite.decline(app, uid);
  }, [app, uid]);

  const hideJoin = useCallback(async () => {
    await api.groups.hide(resource);
  }, [api, resource]);

  const { status } = props;
  if (status) {
    if(status.progress === 'done') {
      return null;
    }
    return (
      <Row gapX="2" alignItems="center" height={4}>
        <StatelessAsyncButton
          height={4}
          backgroundColor="white"
          onClick={hideJoin}
        >
          Cancel
        </StatelessAsyncButton>
      </Row>
    );
  }

  return (
    <Row gapX="2" alignItems="center" height={4}>
      <StatelessAsyncButton
        color="blue"
        height={4}
        backgroundColor="white"
        onClick={inviteAccept}
      >
        Accept
      </StatelessAsyncButton>
      <StatelessAsyncButton
        height={4}
        backgroundColor="white"
        onClick={inviteDecline}
      >
        Decline
      </StatelessAsyncButton>
    </Row>
  );
}

const responsiveStyle = ({ gapXY = 0 as number | number[] }) => {
  return css({
    flexDirection: ['column', 'row'],
    '& > *': {
      marginTop: _.isArray(gapXY) ? [gapXY[0], 0] : [gapXY, 0],
      marginLeft: _.isArray(gapXY) ? [0, ...gapXY.slice(1)] : [0, gapXY]
    },
    '& > :first-child': {
      marginTop: 0,
      marginLeft: 0
    }
  });
};
const ResponsiveRow = styled(Row)(responsiveStyle);
export function GroupInvite(props: GroupInviteProps): ReactElement {
  const { resource, api, preview, invite, status, app, uid } = props;
  const dm = isDm(resource);
  const history = useHistory();

  const invitedTo = dm ? 'DM' : 'group';
  const graphAssoc = useAssocForGraph(resource);

  const headerProps = status
    ? { description: `You are joining a ${invitedTo}` }
    : { description: `invited you to a ${invitedTo}`, authors: [invite!.ship] };

  const onClick = () => {
    if(status?.progress === 'done') {
      const redir = inviteUrl(app !== 'groups', resource, graphAssoc?.metadata);
      if(redir) {
        history.push(redir);
      }
    }
  };

  return (
    <NotificationWrapper api={api}>
      <Header content {...headerProps} />
      <Row onClick={onClick} height={[null, 4]} alignItems="flex-start" gridArea="main">
        <Elbow mx="2" />
        <ResponsiveRow
          gapXY={[1, 2]}
          height={[null, 4]}
          alignItems={['flex-start', 'center']}
        >
          <InviteMetadata preview={preview} resource={resource} />
          <InviteStatus status={status} />
          <InviteActions
            api={api}
            resource={resource}
            status={status}
            app={app}
            uid={uid}
          />
        </ResponsiveRow>
      </Row>
    </NotificationWrapper>
  );
}
