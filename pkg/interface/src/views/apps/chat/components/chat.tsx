import React, { Component } from "react";
import moment from "moment";
import { RouteComponentProps } from "react-router-dom";

import { deSig } from "~/logic/lib/util";
import { ChatHookUpdate } from "~/types/chat-hook-update";
import { Inbox, Envelope } from "~/types/chat-update";
import { Contacts } from "~/types/contact-update";
import { Path, Patp } from "~/types/noun";
import GlobalApi from "~/logic/api/global";
import { Association } from "~/types/metadata-update";
import {Group} from "~/types/group-update";
import { LocalUpdateRemoteContentPolicy } from "~/types";
import { SubmitDragger } from '~/views/components/s3-upload';

import ChatWindow from './lib/ChatWindow';
import ChatHeader from './lib/ChatHeader';
import ChatInput from "./lib/ChatInput";


type ChatScreenProps = RouteComponentProps<{
  ship: Patp;
  station: string;
}> & {
  chatSynced: ChatHookUpdate;
  station: any;
  association: Association;
  api: GlobalApi;
  read: number;
  mailboxSize: number;
  inbox: Inbox;
  contacts: Contacts;
  group: Group;
  pendingMessages: Map<Path, Envelope[]>;
  s3: any;
  popout: boolean;
  sidebarShown: boolean;
  chatInitialized: boolean;
  envelopes: Envelope[];
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
};

interface ChatScreenState {
  messages: Map<string, string>;
  dragover: boolean;
}

export class ChatScreen extends Component<ChatScreenProps, ChatScreenState> {
  private chatInput: React.RefObject<ChatInput>;
  lastNumPending = 0;
  activityTimeout: NodeJS.Timeout | null = null;

  constructor(props) {
    super(props);

    this.state = {
      messages: new Map(),
      dragover: false,
    };

    this.chatInput = React.createRef();

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

  readyToUpload(): boolean {
    return Boolean(this.chatInput.current?.s3Uploader.current?.inputRef.current);
  }

  onDragEnter(event) {
    if (!this.readyToUpload() || !event.dataTransfer.files.length) {
      return;
    }
    this.setState({ dragover: true });
  }

  onDrop(event: DragEvent) {
    this.setState({ dragover: false });
    event.preventDefault();
    if (!event.dataTransfer || !event.dataTransfer.files.length) {
      return;
    }
    if (event.dataTransfer.items.length && !event.dataTransfer.files.length) {
      event.preventDefault();
      return;
    }
    event.preventDefault();
    this.chatInput.current?.uploadFiles(event.dataTransfer.files);
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

    const unreadCount = props.mailboxSize - props.read;
    const unreadMsg = unreadCount > 0 && props.envelopes[unreadCount - 1];
    
    return (
      <div
        key={props.station}
        className="h-100 w-100 overflow-hidden flex flex-column relative"
        onDragEnter={this.onDragEnter.bind(this)}
        onDragOver={event => {
          event.preventDefault();
          if (
            !this.state.dragover
            && (
              (event.dataTransfer.files.length && event.dataTransfer.files[0].kind === 'file')
              || (event.dataTransfer.items.length && event.dataTransfer.items[0].kind === 'file')
            )
          ) {
            this.setState({ dragover: true });
          }
        }}
        onDragLeave={() => this.setState({ dragover: false })}
        onDrop={this.onDrop.bind(this)}
      >
        {this.state.dragover ? <SubmitDragger /> : null}
        <ChatHeader {...props} />
        <ChatWindow
          isChatMissing={isChatMissing}
          isChatLoading={isChatLoading}
          isChatUnsynced={isChatUnsynced}
          unreadCount={unreadCount}
          unreadMsg={unreadMsg}
          stationPendingMessages={pendingMessages}
          ship={props.match.params.ship}
          {...props} />
        <ChatInput
          ref={this.chatInput}
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
          hideAvatars={props.hideAvatars}
        />
      </div>
    );
  }
}
