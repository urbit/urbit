import { JoinRequest } from '@urbit/api';
import { Invite } from '@urbit/api/invite';
import React  from 'react';
import { usePreview } from '~/logic/state/metadata';
import { GroupInvite } from './Group';

interface InviteItemProps {
  invite?: Invite;
  resource: string;
  pendingJoin?: JoinRequest;
  app?: string;
  uid?: string;
}

export function InviteItem(props: InviteItemProps) {
  const { pendingJoin, invite, resource, uid, app } = props;

  const { preview } = usePreview(resource);

  if (pendingJoin?.hidden) {
    return null;
  }

  return (
    <GroupInvite
      resource={resource}
      preview={preview}
      invite={invite}
      status={pendingJoin}
      uid={uid}
      app={app}
    />
  );
  }

export default InviteItem;
