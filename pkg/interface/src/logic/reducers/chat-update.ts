import _ from 'lodash';
import { StoreState } from '~/logic/store/type';
import { Cage } from '~/types/cage';
import { ChatUpdate } from '~/types/chat-update';
import { ChatHookUpdate } from '~/types/chat-hook-update';
import { Envelope } from "~/types/chat-update";

type ChatState = Pick<StoreState, 'chatInitialized' | 'chatSynced' | 'inbox' | 'pendingMessages'>;

export default class ChatReducer<S extends ChatState> {
  reduce(json: Cage, state: S) {
    const data = json['chat-update'];
    if (data) {
      this.initial(data, state);
      this.pending(data, state);
      this.message(data, state);
      this.messages(data, state);
      this.read(data, state);
      this.create(data, state);
      this.delete(data, state);
    }

    const hookUpdate = json['chat-hook-update'];
    if (hookUpdate) {
      this.hook(hookUpdate, state);
    }
  }

  initial(json: ChatUpdate, state: S) {
    const data = _.get(json, 'initial', false);
    if (data) {
      state.inbox = data;
      state.chatInitialized = true;
    }
  }

  hook(json: ChatHookUpdate, state: S) {
    state.chatSynced = json;
  }

  message(json: ChatUpdate, state: S) {
    const data = _.get(json, 'message', false);
    if (data) {
      state.inbox[data.path].envelopes.unshift(data.envelope);
      state.inbox[data.path].config.length
        = state.inbox[data.path].config.length + 1;
    }
  }

  messages(json: ChatUpdate, state: S) {
    const data = _.get(json, 'messages', false);
    if (data) {
      state.inbox[data.path].envelopes = _.unionBy(
        state.inbox[data.path].envelopes,
        data.envelopes,
        (envelope: Envelope) => envelope.uid
      );
    }
  }

  read(json: ChatUpdate, state: S) {
    const data = _.get(json, 'read', false);
    if (data) {
      state.inbox[data.path].config.read =
        state.inbox[data.path].config.length;
    }
  }

  create(json: ChatUpdate, state: S) {
    const data = _.get(json, 'create', false);
    if (data) {
      state.inbox[data.path] = {
        envelopes: [],
        config: {
          read: 0,
          length: 0
        }
      };
    }
  }

  delete(json: ChatUpdate, state: S) {
    const data = _.get(json, 'delete', false);
    if (data) {
      delete state.inbox[data.path];
    }
  }

  pending(json: ChatUpdate, state: S) {
    const msg = _.get(json, 'message', false);
    if (!msg || !state.pendingMessages.has(msg.path)) {
      return;
    }

    const mailbox = state.pendingMessages.get(msg.path) || [];

    for (const pendingMsg of mailbox) {
      if (msg.envelope.uid === pendingMsg.uid) {
        const index = mailbox.indexOf(pendingMsg);
        mailbox.splice(index, 1);
      }
    }
    state.pendingMessages.set(msg.path, mailbox);
  }
}
