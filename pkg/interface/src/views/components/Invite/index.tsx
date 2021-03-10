import React, { useState, useEffect, useCallback } from 'react';
import { useHistory } from 'react-router-dom';

import {
  MetadataUpdatePreview,
  Contacts,
  JoinRequests,
  Groups,
  Associations
} from '@urbit/api';
import { Invite } from '@urbit/api/invite';
import { Text, Icon, Row } from '@tlon/indigo-react';

import { cite } from '~/logic/lib/util';
import GlobalApi from '~/logic/api/global';
import { resourceFromPath } from '~/logic/lib/group';
import { GroupInvite } from './Group';
import { InviteSkeleton } from './InviteSkeleton';
import { JoinSkeleton } from './JoinSkeleton';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';

interface InviteItemProps {
  invite?: Invite;
  resource: string;
  groups: Groups;
  associations: Associations;

  pendingJoin: JoinRequests;
  app?: string;
  uid?: string;
  api: GlobalApi;
  contacts: Contacts;
}

export function InviteItem(props: InviteItemProps) {
  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);
  const { associations, pendingJoin, invite, resource, uid, app, api } = props;
  const { ship, name } = resourceFromPath(resource);
  const waiter = useWaitForProps(props, 50000);
  const status = pendingJoin[resource];

  const history = useHistory();
  const inviteAccept = useCallback(async () => {
    if (!(app && invite && uid)) {
      return;
    }
    if(resource in props.groups) {
      await api.invite.decline(app, uid);
      return;
    }

    api.groups.join(ship, name);
    await waiter(p => resource in p.pendingJoin);

    api.invite.accept(app, uid);
    await waiter((p) => {
      return (
        resource in p.groups &&
        (resource in (p.associations?.graph ?? {}) ||
          resource in (p.associations?.groups ?? {}))
      );
    });

    if (props.groups?.[resource]?.hidden) {
      const { metadata } = associations.graph[resource];
      if (metadata && 'graph' in metadata.config) {
        if (metadata.config.graph === 'chat') {
          history.push(`/~landscape/messages/resource/${metadata.config.graph}${resource}`);
        } else {
          history.push(`/~landscape/home/resource/${metadata.config.graph}${resource}`);
        }
      } else {
        console.error('unknown metadata: ', metadata);
      }
    } else {
      history.push(`/~landscape${resource}`);
    }
  }, [app, invite, uid, resource, props.groups, associations]);

  const inviteDecline = useCallback(async () => {
    if(!(app && uid)) {
      return;
    }
    await api.invite.decline(app, uid);
  }, [app, uid]);

  const handlers = { onAccept: inviteAccept, onDecline: inviteDecline };

  useEffect(() => {
    if (!app || app === 'groups') {
      (async () => {
        setPreview(await api.metadata.preview(resource));
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
        resource={resource}
        api={api}
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
      <JoinSkeleton api={api} resource={resource} status={status} gapY="3">
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
        resource={resource}
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
      <JoinSkeleton api={api} resource={resource} status={status}>
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
