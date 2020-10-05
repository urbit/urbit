import React, { useRef, useCallback } from "react";
import { RouteComponentProps } from "react-router-dom";
import { Col } from "@tlon/indigo-react";

import { Association } from "~/types/metadata-update";
import { StoreState } from "~/logic/store/type";
import { useFileDrag } from "~/logic/lib/useDrag";
import ChatWindow from "./components/ChatWindow";
import ChatInput from "./components/ChatInput";
import GlobalApi from "~/logic/api/global";
import { deSig } from "~/logic/lib/util";
import { SubmitDragger } from "~/views/components/s3-upload";
import { useLocalStorageState } from "~/logic/lib/useLocalStorageState";

type ChatResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function ChatResource(props: ChatResourceProps) {
  const station = props.association["app-path"];
  if (!props.chatInitialized) {
    return null;
  }

  const { envelopes, config } = props.inbox[station];
  const { read, length } = config;

  const groupPath = props.association["group-path"];
  const group = props.groups[groupPath];
  const contacts = props.contacts[groupPath] || {};

  const pendingMessages = (props.pendingMessages.get(station) || []).map(
    (value) => ({
      ...value,
      pending: true,
    })
  );

  const isChatMissing =
    (props.chatInitialized &&
      !(station in props.inbox) &&
      props.chatSynced &&
      !(station in props.chatSynced)) ||
    false;

  const isChatLoading =
    (props.chatInitialized &&
      !(station in props.inbox) &&
      props.chatSynced &&
      station in props.chatSynced) ||
    false;

  const isChatUnsynced =
    (props.chatSynced &&
      !(station in props.chatSynced) &&
      envelopes.length > 0) ||
    false;

  const unreadCount = length - read;
  const unreadMsg = unreadCount > 0 && envelopes[unreadCount - 1];

  const [, owner, name] = station.split("/");
  const ourContact = contacts?.[window.ship];
  const lastMsgNum = envelopes.length || 0;

  const chatInput = useRef<ChatInput>();

  const onFileDrag = useCallback(
    (files: FileList) => {
      if (!chatInput.current) {
        return;
      }
      chatInput.current?.uploadFiles(files);
    },
    [chatInput?.current]
  );

  const { bind, dragging } = useFileDrag(onFileDrag);

  const [unsent, setUnsent] = useLocalStorageState<Record<string, string>>(
    "chat-unsent",
    {}
  );

  const appendUnsent = useCallback(
    (u: string) => setUnsent((s) => ({ ...s, [station]: u })),
    [station]
  );

  const clearUnsent = useCallback(() => setUnsent((s) => _.omit(s, station)), [
    station,
  ]);

  return (
    <Col {...bind} height="100%" overflow="hidden" position="relative">
      {dragging && <SubmitDragger />}
      <ChatWindow
        remoteContentPolicy={props.remoteContentPolicy}
        mailboxSize={length}
        match={props.match as any}
        stationPendingMessages={pendingMessages}
        history={props.history}
        isChatMissing={isChatMissing}
        isChatLoading={isChatLoading}
        isChatUnsynced={isChatUnsynced}
        unreadCount={unreadCount}
        unreadMsg={unreadMsg}
        envelopes={envelopes || []}
        contacts={contacts}
        association={props.association}
        group={group}
        ship={owner}
        station={station}
        allStations={Object.keys(props.inbox)}
        api={props.api}
        hideNicknames={props.hideNicknames}
        hideAvatars={props.hideAvatars}
        location={props.location}
      />
      <ChatInput
        ref={chatInput}
        api={props.api}
        numMsgs={lastMsgNum}
        station={station}
        ourContact={ourContact}
        envelopes={envelopes || []}
        contacts={contacts}
        onUnmount={appendUnsent}
        s3={props.s3}
        hideAvatars={props.hideAvatars}
        placeholder="Message..."
        message={unsent[station] || ""}
        deleteMessage={clearUnsent}
      />
    </Col>
  );
}
