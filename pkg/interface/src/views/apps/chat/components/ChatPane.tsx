import { Col } from '@tlon/indigo-react';
import { Content, Graph, Post } from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import React, { ReactElement, useCallback, useEffect, useState } from 'react';
import create from 'zustand';
import { persist } from 'zustand/middleware';
import { useFileUpload } from '~/logic/lib/useFileUpload';
import { createStorageKey, storageVersion, clearStorageMigration } from '~/logic/lib/util';
import { useOurContact } from '~/logic/state/contact';
import { useGraphTimesent } from '~/logic/state/graph';
import { Loading } from '~/views/components/Loading';
import SubmitDragger from '~/views/components/SubmitDragger';
import ChatInput from './ChatInput';
import ChatWindow from './ChatWindow';

interface useChatStoreType {
  id: string;
  message: string;
  messageStore: Record<string, string>;
  restore: (id: string) => void;
  setMessage: (message: string) => void;
}

export const useChatStore = create<useChatStoreType>(persist((set, get) => ({
  id: '',
  message: '',
  messageStore: {},
  restore: (id: string) => {
    const store = get().messageStore;
    set({
      id,
      messageStore: store,
      message: store[id] || ''
    });
  },
  setMessage: (message: string) => {
    const store = get().messageStore;
    store[get().id] = message;

    set({ message, messageStore: store });
  }
}), {
  whitelist: ['messageStore'],
  name: createStorageKey('chat-unsent'),
  version: storageVersion,
  migrate: clearStorageMigration
}));

interface ChatPaneProps {
  /**
   * A key to uniquely identify a ChatPane instance. Should be either the
   * resource for group chats or the @p for DMs
   */
  id: string;
  /**
   * The graph of the chat to render
   */
  graph: Graph;
  unreadCount: number;
  /**
   * User able to write to chat
   */
  canWrite: boolean;
  /**
   * Get contents of reply message
   */
  onReply: (msg: Post) => string;
  onDelete?: (msg: Post) => void;
  /**
   * Fetch more messages
   *
   * @param newer Get newer or older backlog
   * @returns Whether backlog is finished loading in that direction
   */
  fetchMessages: (newer: boolean) => Promise<boolean>;
  /**
   * Dismiss unreads for chat
   */
  dismissUnread: () => void;
  /**
   * Get permalink for a node
   */
  getPermalink: (idx: BigInteger) => string;
  isAdmin: boolean;
  /**
   * Post message with contents to channel
   */
  onSubmit: (contents: Content[]) => void;
  /**
   *
   * Users or group we haven't shared our contact with yet
   *
   * string[] - array of ships
   * string - path of group
   */
  promptShare?: string[] | string;
}

export function ChatPane(props: ChatPaneProps): ReactElement {
  const {
    graph,
    unreadCount,
    canWrite,
    id,
    getPermalink,
    isAdmin,
    dismissUnread,
    onSubmit,
    onDelete,
    promptShare = [],
    fetchMessages
  } = props;
  const graphTimesentMap = useGraphTimesent(id);
  const ourContact = useOurContact();
  const { restore, setMessage } = useChatStore(s => ({ setMessage: s.setMessage, restore: s.restore }));
  const [uploadError, setUploadError] = useState<string>('');

  const handleUploadError = useCallback((err: Error) => {
    setUploadError(err.message);
  }, []);

  const { canUpload, drag } = useFileUpload({
    onSuccess: (url) => {
      onSubmit([{ url }]);
      setUploadError('');
    },
    onError: handleUploadError
  });

  useEffect(() => {
    restore(id);
  }, [id]);

  const scrollTo = new URLSearchParams(location.search).get('msg');

  const [showBanner, setShowBanner] = useState(false);

  useEffect(() => {
    setShowBanner(promptShare.length > 0);
  }, [promptShare]);

  const onReply = useCallback(
    (msg: Post) => {
      const message = props.onReply(msg);
      setMessage(message);
    },
    [id, props.onReply]
  );

  if (!graph) {
    return <Loading />;
  }

  return (
    // @ts-ignore bind typings
    <Col {...drag.bind} height="100%" overflow="hidden" position="relative">
      {canUpload && drag.dragging && <SubmitDragger />}
      <ChatWindow
        key={id}
        graph={graph}
        graphSize={graph.size}
        unreadCount={unreadCount}
        showOurContact={promptShare.length === 0 && !showBanner}
        pendingSize={Object.keys(graphTimesentMap).length}
        onReply={onReply}
        onDelete={onDelete}
        dismissUnread={dismissUnread}
        fetchMessages={fetchMessages}
        isAdmin={isAdmin}
        getPermalink={getPermalink}
        scrollTo={scrollTo ? bigInt(scrollTo) : undefined}
      />
      {canWrite && (
        <ChatInput
          onSubmit={onSubmit}
          ourContact={(promptShare.length === 0 && ourContact) || undefined}
          placeholder="Message..."
          uploadError={uploadError}
          setUploadError={setUploadError}
          handleUploadError={handleUploadError}
        />
      )}
    </Col>
  );
}

ChatPane.whyDidYouRender = true;
