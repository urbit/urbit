import { Content, createPost } from '@urbit/api';
import React, { useCallback } from 'react';
import { patp2dec } from 'urbit-ob';
import GlobalApi from '~/logic/api/global';
import { useDM } from '~/logic/state/graph';
import useHarkState, { useHarkDm } from '~/logic/state/hark';
import { ChatPane } from './components/ChatPane';

interface DmResourceProps {
  ship: string;
  api: GlobalApi;
}
export function DmResource(props: DmResourceProps) {
  const { ship, api } = props;
  const dm = useDM(ship);
  const hark = useHarkDm(ship);
  const unreadCount = (hark?.unreads as number) ?? 0;

  const onSubmit = useCallback(
    (contents: Content[]) => {
      api.graph.addDmMessage(ship, contents);
    },
    [ship]
  );

  return (
    <ChatPane
      api={api}
      canWrite
      id={ship}
      graph={dm}
      unreadCount={unreadCount}
      onReply={() => ''}
      fetchMessages={async () => true}
      dismissUnread={() => {}}
      getPermalink={() => ''}
      isAdmin
      onSubmit={onSubmit}
    />
  );
}
