import {
  JoinRequest, MetadataUpdatePreview
} from '@urbit/api';
import { Invite } from '@urbit/api/invite';
import React, { useEffect, useState } from 'react';
import GlobalApi from '~/logic/api/global';
import { GroupInvite } from './Group';

interface InviteItemProps {
  invite?: Invite;
  resource: string;
  pendingJoin?: JoinRequest;
  app?: string;
  uid?: string;
  api: GlobalApi;
}

export function InviteItem(props: InviteItemProps) {
  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);
  const { pendingJoin, invite, resource, uid, app, api } = props;

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

  if (pendingJoin?.hidden) {
    return null;
  }

  return (
    <GroupInvite
      resource={resource}
      api={api}
      preview={preview}
      invite={invite}
      status={pendingJoin}
      uid={uid}
      app={app}
    />
  );
  }

export default InviteItem;
