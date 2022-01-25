import { Col } from '@tlon/indigo-react';
import { Association, Content, Graph, Group, Post } from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import React, { ReactElement, useCallback, useEffect, useState, useRef } from 'react';
import create from 'zustand';
import { persist } from 'zustand/middleware';
import { useFileUpload } from '~/logic/lib/useFileUpload';
import { createStorageKey, storageVersion, clearStorageMigration } from '~/logic/lib/util';
import { useOurContact } from '~/logic/state/contact';
import { useGraphTimesent } from '~/logic/state/graph';
import ShareProfile from '~/views/apps/chat/components/ShareProfile';
import { Loading } from '~/views/components/Loading';
import SubmitDragger from '~/views/components/SubmitDragger';
import ChatInput from './ChatInput';
import ChatWindow from './ChatWindow';
import { CodeMirrorShim } from './ChatEditor';
import { IS_MOBILE } from '~/logic/lib/platform';


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
  group: Group;
  association: Association;
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
  onLike?: (msg: Post) => void;
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
    group,
    association,
    unreadCount,
    canWrite,
    id,
    getPermalink,
    isAdmin,
    dismissUnread,
    onSubmit,
    onDelete,
    onLike,
    promptShare = [],
    fetchMessages
  } = props;
  const graphTimesentMap = useGraphTimesent(id);
  const ourContact = useOurContact();
  const { message, restore, setMessage } = useChatStore();
  const { canUpload, drag } = useFileUpload({
    onSuccess: url => onSubmit([{ url }])
  });
  const inputRef = useRef<CodeMirrorShim>(null);
  const scrollTo = new URLSearchParams(location.search).get('msg');
  const [showBanner, setShowBanner] = useState(false);

  useEffect(() => {
    restore(id);
    if (!IS_MOBILE) {
      inputRef.current?.focus();
    }
  }, [id]);

  useEffect(() => {
    setShowBanner(promptShare.length > 0);
  }, [promptShare]);

  const onReply = useCallback(
    (msg: Post) => {
      // Prepend the reply reference
      setMessage(`${props.onReply(msg)}${message || ''}`);
      inputRef.current.focus();
    },
    [id, message, props.onReply]
  );

  if (!graph) {
    return <Loading />;
  }

  return (
    // @ts-ignore bind typings
    <Col {...drag.bind} height="100%" overflow="hidden" position="relative">
      <ShareProfile
        our={ourContact}
        recipients={showBanner ? promptShare : []}
        onShare={() => setShowBanner(false)}
      />
      {canUpload && drag.dragging && <SubmitDragger />}
      <ChatWindow
        key={id}
        graphSize={graph.size}
        showOurContact={promptShare.length === 0 && !showBanner}
        pendingSize={Object.keys(graphTimesentMap).length}
        scrollTo={scrollTo ? bigInt(scrollTo) : undefined}
        {...{ graph, unreadCount, onReply, onDelete, onLike, dismissUnread, fetchMessages, isAdmin, getPermalink }}
      />
      {canWrite && (
        <ChatInput
          {...{ onSubmit, isAdmin, group, association }}
          ourContact={(promptShare.length === 0 && ourContact) || undefined}
          placeholder="Message..."
          chatEditor={inputRef}
        />
      )}
    </Col>
  );
}

ChatPane.whyDidYouRender = true;
