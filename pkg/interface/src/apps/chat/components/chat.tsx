import React, { Component, Fragment } from "react";
import moment from "moment";

import { Link, RouteComponentProps } from "react-router-dom";

import { ChatWindow } from './lib/chat-window';
import { ChatHeader } from './lib/chat-header';
import { ChatInput } from "./lib/chat-input";
import { deSig } from "../../../lib/util";
import { ChatHookUpdate } from "../../../types/chat-hook-update";
import ChatApi from "../../../api/chat";
import { Inbox, Envelope } from "../../../types/chat-update";
import { Contacts } from "../../../types/contact-update";
import { Path, Patp } from "../../../types/noun";
import GlobalApi from "../../../api/global";
import { Association } from "../../../types/metadata-update";
import {Group} from "../../../types/group-update";

const ACTIVITY_TIMEOUT = 60000; // a minute


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
  read: number;
  active: boolean;
  messages: Map<string, string>;
}

export class ChatScreen extends Component<ChatScreenProps, ChatScreenState> {
  lastNumPending = 0;
  activityTimeout: NodeJS.Timeout | null = null;

  constructor(props) {
    super(props);

    this.state = {
      active: true,
      messages: new Map(),
    };

    this.handleActivity = this.handleActivity.bind(this);
    this.setInactive = this.setInactive.bind(this);

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

  componentDidMount() {
    document.addEventListener("mousemove", this.handleActivity, false);
    document.addEventListener("mousedown", this.handleActivity, false);
    document.addEventListener("keypress", this.handleActivity, false);
    document.addEventListener("touchmove", this.handleActivity, false);
    this.activityTimeout = setTimeout(this.setInactive, ACTIVITY_TIMEOUT);
  }

  componentWillUnmount() {
    document.removeEventListener("mousemove", this.handleActivity, false);
    document.removeEventListener("mousedown", this.handleActivity, false);
    document.removeEventListener("keypress", this.handleActivity, false);
    document.removeEventListener("touchmove", this.handleActivity, false);
    if (this.activityTimeout) {
      clearTimeout(this.activityTimeout);
    }
  }

  handleActivity() {
    if (!this.state.active) {
      this.setState({ active: true });
    }

    if (this.activityTimeout) {
      clearTimeout(this.activityTimeout);
    }

    this.activityTimeout = setTimeout(this.setInactive, ACTIVITY_TIMEOUT);
  }

  setInactive() {
    this.activityTimeout = null;
    this.setState({ active: false, scrollLocked: true });
  }

  render() {
    const { props, state } = this;

    const lastMsgNum = props.envelopes.length > 0 ? props.envelopes.length : 0;
    const ownerContact =
      window.ship in props.contacts ? props.contacts[window.ship] : false;

    return (
      <div
        key={props.station}
        className="h-100 w-100 overflow-hidden flex flex-column relative"
      >
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
          match={props.match}
          api={props.api}
          group={props.group}
          contacts={props.contacts}
          inbox={props.inbox}
          pendingMessages={props.pendingMessages}
          envelopes={props.envelopes}
          station={props.station}
          chatSynced={props.chatSynced}
          chatInitialized={props.chatInitialized}
          length={props.length}
          read={props.read}
          active={state.active} />
        <ChatInput
          api={props.api}
          numMsgs={lastMsgNum}
          station={props.station}
          owner={deSig(props.match.params.ship)}
          ownerContact={ownerContact}
          envelopes={props.envelopes}
          contacts={props.contacts}
          onEnter={() => this.setState({ scrollLocked: false })}
          onUnmount={(msg: string) => this.setState({
            messages: this.state.messages.set(props.station, msg)
          })}
          s3={props.s3}
          placeholder="Message..."
          message={this.state.messages.get(props.station) || ""}
        />
      </div>
    );
  }
}
