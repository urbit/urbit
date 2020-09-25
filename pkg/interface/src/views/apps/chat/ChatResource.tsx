import React from "react";
import { RouteComponentProps } from "react-router-dom";
import { Col } from "@tlon/indigo-react";

import { Association } from "~/types/metadata-update";
import { StoreState } from "~/logic/store/type";
import ChatWindow from "./components/lib/ChatWindow";
import ChatInput from "./components/lib/ChatInput";
import GlobalApi from "~/logic/api/global";
import { deSig } from "~/logic/lib/util";

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

  const pendingMessages = (props.pendingMessages.get(props.station) || []).map(
    (value) => ({
      ...value,
      pending: true,
    })
  );

  const isChatMissing =
    props.chatInitialized &&
    !(station in props.inbox) &&
    props.chatSynced &&
    !(station in props.chatSynced) || false;

  const isChatLoading =
    props.chatInitialized &&
    !(station in props.inbox) &&
    props.chatSynced &&
    station in props.chatSynced || false;

  const isChatUnsynced =
    props.chatSynced && !(station in props.chatSynced) && envelopes.length > 0 || false;

  const unreadCount = length - read;
  const unreadMsg = unreadCount > 0 && envelopes[unreadCount - 1];




  const [, owner, name] = station.split("/");
  const ownerContact = contacts?.[deSig(owner)];
  const lastMsgNum = 0;

  return (
    <Col height="100%" overflow="hidden" position="relative">
      <ChatWindow
        remoteContentPolicy={props.remoteContentPolicy}
        mailboxSize={length}
        match={props.match as any}
        stationPendingMessages={[]}
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
        api={props.api}
        hideNicknames={props.hideNicknames}
        hideAvatars={props.hideAvatars}
        location={props.location}
      />
      <ChatInput
        api={props.api}
        numMsgs={lastMsgNum}
        station={station}
        owner={deSig(owner)}
        ownerContact={ownerContact}
        envelopes={envelopes || []}
        contacts={contacts}
        onUnmount={(msg: string) => {
          /*this.setState({
            messages: this.state.messages.set(props.station, msg),
          }) */
        }}
        s3={props.s3}
        hideAvatars={props.hideAvatars}
        placeholder="Message..."
        message={"" || ""}
        deleteMessage={() => {}}
      />
    </Col>
  );
}
