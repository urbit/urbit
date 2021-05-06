import { Col } from '@tlon/indigo-react';
import { Content, Graph, Post } from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import _ from 'lodash';
import React, { ReactElement, useCallback, useEffect, useRef, useState } from 'react';
import GlobalApi from '~/logic/api/global';
import { useFileDrag } from '~/logic/lib/useDrag';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { useOurContact } from '~/logic/state/contact';
import useGraphState from '~/logic/state/graph';
import ShareProfile from '~/views/apps/chat/components/ShareProfile';
import { Loading } from '~/views/components/Loading';
import SubmitDragger from '~/views/components/SubmitDragger';
import ChatInput from './ChatInput';
import ChatWindow from './ChatWindow';

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
  api: GlobalApi;
  /**
   * Get contents of reply message
   */
  onReply: (msg: Post) => string;
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
    api,
    graph,
    unreadCount,
    canWrite,
    id,
    getPermalink,
    isAdmin,
    dismissUnread,
    onSubmit,
    promptShare = [],
    fetchMessages
  } = props;
  const graphTimesentMap = useGraphState(state => state.graphTimesentMap);
  const ourContact = useOurContact();
  const chatInput = useRef<ChatInput>();

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
      setUnsent(s => ({ ...s, [id]: message }));
    },
    [id, props.onReply]
  );

  if (!graph) {
    return <Loading />;
  }

  return (
    <Col {...bind} height="100%" overflow="hidden" position="relative">
      <ShareProfile
        our={ourContact}
        api={api}
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
        pendingSize={Object.keys(graphTimesentMap[id] || {}).length}
        onReply={onReply}
        dismissUnread={dismissUnread}
        fetchMessages={fetchMessages}
        isAdmin={isAdmin}
        getPermalink={getPermalink}
        api={api}
        scrollTo={scrollTo ? bigInt(scrollTo) : undefined}
      />
      {canWrite && (
        <ChatInput
          ref={chatInput}
          api={props.api}
          onSubmit={onSubmit}
          ourContact={(promptShare.length === 0 && ourContact) || undefined}
          onUnmount={appendUnsent}
          placeholder="Message..."
          message={unsent[id] || ''}
          deleteMessage={clearUnsent}
        />
      )}
    </Col>
  );
}
