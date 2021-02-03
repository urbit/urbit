import React, { useRef, useCallback, useEffect, useState } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import { Col } from '@tlon/indigo-react';
import _ from 'lodash';

import { Association } from '~/types/metadata-update';
import { StoreState } from '~/logic/store/type';
import { useFileDrag } from '~/logic/lib/useDrag';
import ChatWindow from './components/ChatWindow';
import ChatInput from './components/ChatInput';
import GlobalApi from '~/logic/api/global';
import { ShareProfile } from '~/views/apps/chat/components/ShareProfile';
import SubmitDragger from '~/views/components/SubmitDragger';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { Loading } from '~/views/components/Loading';
import useS3 from '~/logic/lib/useS3';
import {isWriter} from '~/logic/lib/group';

type ChatResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function ChatResource(props: ChatResourceProps) {
  const station = props.association.resource;
  const groupPath = props.association.group;
  const group = props.groups[groupPath];
  const contacts = props.contacts;

  const graph = props.graphs[station.slice(7)];

  const isChatMissing = !props.graphKeys.has(station.slice(7));

  const unreadCount = props.unreads.graph?.[station]?.['/']?.unreads || 0;

  const [,, owner, name] = station.split('/');
  const ourContact = contacts?.[`~${window.ship}`];
  console.log(contacts);

  const chatInput = useRef<ChatInput>();

  const canWrite = isWriter(group, station);

  useEffect(() => {
    const count = Math.min(50, unreadCount + 15);
    props.api.graph.getNewest(owner, name, count);
  }, [station]);

  const onFileDrag = useCallback(
    (files: FileList | File[]) => {
      if (!chatInput.current) {
        return;
      }
      chatInput.current?.uploadFiles(files);
    },
    [chatInput.current]
  );

  const { bind, dragging } = useFileDrag(onFileDrag);

  const [unsent, setUnsent] = useLocalStorageState<Record<string, string>>(
    'chat-unsent',
    {}
  );

  const appendUnsent = useCallback(
    (u: string) => setUnsent(s => ({ ...s, [station]: u })),
    [station]
  );

  const clearUnsent = useCallback(
    () => setUnsent(s => _.omit(s, station)),
    [station]
  );

  const scrollTo = new URLSearchParams(location.search).get('msg');

  useEffect(() => {
    const clear = () => {
      props.history.replace(location.pathname);
    };
    setTimeout(clear, 10000);
    return clear;
  }, [station]);

  if(!graph) {
    return <Loading />;
  }

  return (
    <Col {...bind} height="100%" overflow="hidden" position="relative">
      <ShareProfile
        our={ourContact}
        api={props.api}
        recipient={owner}
        group={group}
        groupPath={groupPath}
       />
      {dragging && <SubmitDragger />}
      <ChatWindow
        mailboxSize={5}
        match={props.match as any}
        stationPendingMessages={[]}
        history={props.history}
        isChatMissing={false}
        isChatLoading={false}
        isChatUnsynced={false}
        graph={graph}
        unreadCount={unreadCount}
        unreadMsg={false}
        envelopes={[]}
        contacts={contacts}
        association={props.association}
        group={group}
        ship={owner}
        station={station}
        api={props.api}
        location={props.location}
        scrollTo={scrollTo ? parseInt(scrollTo, 10) : undefined}
      />
      { canWrite && (
      <ChatInput
        ref={chatInput}
        api={props.api}
        station={station}
        ourContact={ourContact}
        envelopes={[]}
        contacts={contacts}
        onUnmount={appendUnsent}
        s3={props.s3}
        placeholder="Message..."
        message={unsent[station] || ''}
        deleteMessage={clearUnsent}
      /> )}
    </Col>
  );
}
