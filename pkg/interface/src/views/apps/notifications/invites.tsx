import React, { ReactElement } from 'react';
import _ from 'lodash';

import {
  Invite,
  AppInvites,
  JoinRequest
} from '@urbit/api';
import { alphabeticalOrder, resourceAsPath } from '~/logic/lib/util';
import useInviteState from '~/logic/state/invite';
import useGraphState from '~/logic/state/graph';
import { PendingDm } from './PendingDm';
import InviteItem from '~/views/components/Invite';

interface InvitesProps {
  pendingJoin?: any;
}

interface InviteRef {
  uid: string;
  app: string;
  invite: Invite;
}

export function Invites(props: InvitesProps): ReactElement {
  const invites = useInviteState(state => state.invites);

  const pendingDms = useGraphState(s => s.pendingDms) ?? [];

  const inviteArr: InviteRef[] = _.reduce(
    invites,
    (acc: InviteRef[], val: AppInvites, app: string) => {
      const appInvites = _.reduce(
        val,
        (invs: InviteRef[], invite: Invite, uid: string) => {
          return [...invs, { invite, uid, app }];
        },
        []
      );
      return [...acc, ...appInvites];
    },
    []
  );

  const pendingJoin = _.omitBy(props.pendingJoin, 'hidden');

  const invitesAndStatus: { [rid: string]: JoinRequest | InviteRef } = {
    ..._.keyBy(inviteArr, ({ invite }) => resourceAsPath(invite.resource)),
    ...pendingJoin
  };

  return (
    <>
      {[...pendingDms].map(ship => (
        <PendingDm key={ship} ship={`~${ship}`} />
      ))}
      {Object.keys(invitesAndStatus)
        .sort(alphabeticalOrder)
        .map((resource) => {
          const inviteOrStatus = invitesAndStatus[resource];
          const join = pendingJoin[resource];
          if ('progress' in inviteOrStatus) {
            return (
              <InviteItem
                key={resource}
                resource={resource}
                pendingJoin={join}
              />
            );
          } else {
            const { app, uid, invite } = inviteOrStatus;
            return (
              <InviteItem
                key={resource}
                invite={invite}
                app={app}
                uid={uid}
                resource={resource}
              />
            );
          }
        })}
    </>
  );
}
