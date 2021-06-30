import { Col } from '@tlon/indigo-react';
import { Content, Graph, Post } from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import _ from 'lodash';
import React, { ReactElement, useCallback, useEffect, useRef, useState } from 'react';
import create from 'zustand';
import { useFileDrag } from '~/logic/lib/useDrag';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { useOurContact } from '~/logic/state/contact';
import { useGraphTimesent } from '~/logic/state/graph';
import ShareProfile from '~/views/apps/chat/components/ShareProfile';
import { Loading } from '~/views/components/Loading';
import SubmitDragger from '~/views/components/SubmitDragger';
import ChatInput, { ChatInput as NakedChatInput } from './ChatInput';
import ChatWindow from './ChatWindow';

interface useChatStoreType {
  message: string;
  setMessage: (message: string) => void;
}

export const useChatStore = create<useChatStoreType>(set => ({
  message: '',
  setMessage: (message: string) => set({ message })
}));

// const unsentKey = 'chat-unsent';

// export const useChatStore = create<useChatStoreType>(set => ({
//   message: retrieve(unsentKey, ''),
//   setMessage: (message: string) => {
//     set({ message });
//     localStorage.setItem(unsentKey, message);
//   }
// }));

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
  const chatInput = useRef<NakedChatInput>();
  const setMessage = useChatStore(s => s.setMessage);

  const onFileDrag = useCallback(
    (files: FileList | File[]) => {
      if (!chatInput.current) {
        return;
      }
      (chatInput.current as NakedChatInput)?.uploadFiles(files);
    },
    [chatInput]
  );

  const { bind, dragging } = useFileDrag(onFileDrag);

  const [, setUnsent] = useLocalStorageState<Record<string, string>>(
    'chat-unsent',
    {}
  );

  const appendUnsent = useCallback(
    (u: string) => setUnsent(s => ({ ...s, [id]: u })),
    [id]
  );

  const clearUnsent = useCallback(() => {
    setUnsent((s) => {
      if (id in s) {
        return _.omit(s, id);
      }
      return s;
    });
  }, [id]);

  const scrollTo = new URLSearchParams(location.search).get('msg');

  const [showBanner, setShowBanner] = useState(false);

  useEffect(() => {
    setShowBanner(promptShare.length > 0);
  }, [promptShare]);

  const onReply = useCallback(
    (msg: Post) => {
      const message = props.onReply(msg);
      console.log(message);
      // setUnsent(s => ({ ...s, [id]: message }));
      setMessage(message);
    },
    [id, props.onReply]
  );

  if (!graph) {
    return <Loading />;
  }

  return (
    // @ts-ignore bind typings
    <Col {...bind} height="100%" overflow="hidden" position="relative">
      <ShareProfile
        our={ourContact}
        recipients={showBanner ? promptShare : []}
        onShare={() => setShowBanner(false)}
      />
      {dragging && <SubmitDragger />}
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
          ref={chatInput}
          onSubmit={onSubmit}
          ourContact={(promptShare.length === 0 && ourContact) || undefined}
          onUnmount={appendUnsent}
          placeholder="Message..."
          deleteMessage={clearUnsent}
        />
      )}
    </Col>
  );
}

ChatPane.whyDidYouRender = true;
