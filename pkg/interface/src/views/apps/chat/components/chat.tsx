import React, { Component, Fragment } from "react";
import moment from "moment";

import { Link, RouteComponentProps } from "react-router-dom";

import { ChatWindow } from './lib/chat-window';
import { ChatHeader } from './lib/chat-header';
import { ChatInput } from "./lib/chat-input";
import { deSig } from "~/logic/lib/util";
import { ChatHookUpdate } from "~/types/chat-hook-update";
import ChatApi from "~/logic/api/chat";
import { Inbox, Envelope } from "~/types/chat-update";
import { Contacts } from "~/types/contact-update";
import { Path, Patp } from "~/types/noun";
import GlobalApi from "~/logic/api/global";
import { Association } from "~/types/metadata-update";
import {Group} from "~/types/group-update";


type ChatScreenProps = RouteComponentProps<{
  ship: Patp;
  station: string;
}> & {
  chatSynced: ChatHookUpdate;
  station: any;
  association: Association;
  api: GlobalApi;
  read: number;
  length: number;
  inbox: Inbox;
  contacts: Contacts;
  group: Group;
  pendingMessages: Map<Path, Envelope[]>;
  s3: any;
  popout: boolean;
  sidebarShown: boolean;
  chatInitialized: boolean;
  envelopes: Envelope[];
};

interface ChatScreenState {
  messages: Map<string, string>;
}

export class ChatScreen extends Component<ChatScreenProps, ChatScreenState> {
  lastNumPending = 0;
  activityTimeout: NodeJS.Timeout | null = null;

  constructor(props) {
    super(props);

    this.state = {
      messages: new Map(),
    };

    moment.updateLocale("en", {
      calendar: {
        sameDay: "[Today]",
        nextDay: "[Tomorrow]",
        nextWeek: "dddd",
        lastDay: "[Yesterday]",
        lastWeek: "[Last] dddd",
        sameElse: "DD/MM/YYYY",
      },
    });
  }

  render() {
    const { props, state } = this;

    const lastMsgNum = props.envelopes.length > 0 ? props.envelopes.length : 0;
    const ownerContact =
      window.ship in props.contacts ? props.contacts[window.ship] : false;

    const pendingMessages = (props.pendingMessages.get(props.station) || [])
      .map((value) => ({
        ...value,
        pending: true
      }));

    const isChatMissing =
      props.chatInitialized &&
      !(props.station in props.inbox) &&
      props.chatSynced &&
      !(props.station in props.chatSynced);

    const isChatLoading =
      props.chatInitialized &&
      !(props.station in props.inbox) &&
      props.chatSynced &&
      (props.station in props.chatSynced);

    const isChatUnsynced =
      props.chatSynced &&
      !(props.station in props.chatSynced) &&
      props.envelopes.length > 0;

    const unreadCount = props.length - props.read;
    const unreadMsg = unreadCount > 0 && props.envelopes[unreadCount - 1];

    return (
      <div
        key={props.station}
        className="h-100 w-100 overflow-hidden flex flex-column relative">
        <ChatHeader
          match={props.match}
          location={props.location}
          api={props.api}
          group={props.group}
          association={props.association}
          station={props.station}
          sidebarShown={props.sidebarShown}
          popout={props.popout} />
        <ChatWindow
          history={props.history}
          isChatMissing={isChatMissing}
          isChatLoading={isChatLoading}
          isChatUnsynced={isChatUnsynced}
          unreadCount={unreadCount}
          unreadMsg={unreadMsg}
          pendingMessages={pendingMessages}
          messages={props.envelopes}
          length={props.length}
          contacts={props.contacts}
          association={props.association}
          group={props.group}
          ship={props.match.params.ship}
          station={props.station}
          api={props.api} />
        <ChatInput
          api={props.api}
          numMsgs={lastMsgNum}
          station={props.station}
          owner={deSig(props.match.params.ship)}
          ownerContact={ownerContact}
          envelopes={props.envelopes}
          contacts={props.contacts}
          onUnmount={(msg: string) => this.setState({
            messages: this.state.messages.set(props.station, msg)
          })}
          s3={props.s3}
          placeholder="Message..."
          message={this.state.messages.get(props.station) || ""}
          deleteMessage={() => this.setState({
            messages: this.state.messages.set(props.station, "")
          })}
        />
      </div>
    );
  }
}
