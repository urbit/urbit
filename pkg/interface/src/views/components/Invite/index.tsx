import React, { useState, useEffect, useCallback } from 'react';
import { useHistory } from 'react-router-dom';

import {
  MetadataUpdatePreview,
  Contacts,
  JoinRequests,
  Groups,
  Associations,
  decline,
  accept,
  join,
  invite as inviteApi,
  groups as groupsApi
} from '@urbit/api';
import { Invite } from '@urbit/api/invite';
import { Text, Icon, Row } from '@tlon/indigo-react';

import { cite } from '~/logic/lib/util';
import { resourceFromPath } from '~/logic/lib/group';
import { GroupInvite } from './Group';
import { InviteSkeleton } from './InviteSkeleton';
import { JoinSkeleton } from './JoinSkeleton';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import useGroupState from '~/logic/state/group';
import useMetadataState from '~/logic/state/metadata';
import useApi from '~/logic/api';

interface InviteItemProps {
  invite?: Invite;
  resource: string;
  app?: string;
  uid?: string;
}

export function InviteItem(props: InviteItemProps) {
  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);
  const { invite, resource, uid, app } = props;
  const { ship, name } = resourceFromPath(resource);
  const pendingJoin = useGroupState(state => state.pendingJoin);
  const status = pendingJoin[resource];
  const groups = useGroupState(state => state.groups);
  const associations = useMetadataState(state => state.associations);
  const getPreview = useMetadataState(state => state.preview);
  const waiter = useWaitForProps({ associations, groups, pendingJoin }, 50000);
  const api = useApi();

  const history = useHistory();
  const inviteAccept = useCallback(async () => {
    if (!(app && invite && uid)) {
      return;
    }
    if (resource in groups) {
      await api.poke(inviteApi.decline(app, uid));
      return;
    }

    api.poke(groupsApi.join(ship, name));
    await waiter(p => resource in p.pendingJoin);

    api.poke(inviteApi.accept(app, uid));
    await waiter((p) => {
      return (
        resource in p.groups &&
        (resource in (p.associations?.graph ?? {}) ||
          resource in (p.associations?.groups ?? {}))
      );
    });

    if (groups?.[resource]?.hidden) {
      const { metadata } = associations.graph[resource];
      if (metadata?.module === 'chat') {
        history.push(`/~landscape/messages/resource/${metadata.module}${resource}`);
      } else {
        history.push(`/~landscape/home/resource/${metadata.module}${resource}`);
      }
    } else {
      history.push(`/~landscape${resource}`);
    }
  }, [app, invite, uid, resource, groups, associations]);

  const inviteDecline = useCallback(async () => {
    if(!(app && uid)) {
      return;
    }
    await api.poke(inviteApi.decline(app, uid));
  }, [app, uid]);

  const handlers = { onAccept: inviteAccept, onDecline: inviteDecline };

  useEffect(() => {
    if (!app || app === 'groups') {
      (async () => {
        setPreview(await getPreview(resource));
      })();
      return () => {
        setPreview(null);
      };
    } else {
      return () => {};
    }
  }, [invite]);

  if (preview) {
    return (
      <GroupInvite
        preview={preview}
        invite={invite}
        status={status}
        {...handlers}
      />
    );
  } else if (invite && name.startsWith('dm--')) {
    return (
      <InviteSkeleton
        gapY="3"
        {...handlers}
        acceptDesc="Join DM"
        declineDesc="Decline DM"
      >
        <Row py="1" alignItems="center">
          <Icon display="block" color="blue" icon="Bullet" mr="2" />
          <Text mr="1" mono>
            {cite(`~${invite!.ship}`)}
          </Text>
          <Text mr="1">invited you to a DM</Text>
        </Row>
      </InviteSkeleton>
    );
  } else if (status && name.startsWith('dm--')) {
    return (
      <JoinSkeleton status={status} gapY="3">
        <Row py="1" alignItems="center">
          <Icon display="block" color="blue" icon="Bullet" mr="2" />
          <Text mr="1">Joining direct message...</Text>
        </Row>
      </JoinSkeleton>
    );
  } else if (invite) {
    return (
      <InviteSkeleton
        acceptDesc="Accept Invite"
        declineDesc="Decline Invite"
        {...handlers}
        gapY="3"
      >
        <Row py="1" alignItems="center">
          <Icon display="block" color="blue" icon="Bullet" mr="2" />
          <Text mr="1" mono>
            {cite(`~${invite!.ship}`)}
          </Text>
          <Text mr="1">
            invited you to ~{invite.resource.ship}/{invite.resource.name}
          </Text>
        </Row>
      </InviteSkeleton>
    );
  } else if (status) {
    const [, , ship, name] = resource.split('/');
    return (
      <JoinSkeleton status={status}>
        <Row py="1" alignItems="center">
          <Icon display="block" color="blue" icon="Bullet" mr="2" />
          <Text mr="1">
            You are joining
          </Text>
          <Text mono>
            {cite(ship)}/{name}
          </Text>
        </Row>
      </JoinSkeleton>
    );
  }
  return null;
}

export default InviteItem;
