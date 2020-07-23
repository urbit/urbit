import BaseApi from './base';
import { uuid } from '../lib/util';
import { Letter, ChatAction, Envelope } from '../types/chat-update';
import { Patp, Path, PatpNoSig } from '../types/noun';
import { StoreState } from '../store/type';
import BaseStore from '../store/base';


export default class ChatApi extends BaseApi<StoreState> {

  /**
   * Fetch backlog
   */
  fetchMessages(start: number, end: number, path: Path) {
    fetch(`/chat-view/paginate/${start}/${end}${path}`)
      .then(response => response.json())
      .then((json) => {
        this.store.handleEvent({
          data: json
        });
      });
  }

  /**
   * Send a message to the chat at path
   */
  message(path: Path, author: Patp, when: string, letter: Letter): Promise<void> {
    const data: ChatAction = {
      message: {
        path,
        envelope: {
          uid: uuid(),
          number: 0,
          author,
          when,
          letter
        }
      }
    };

    const promise = this.proxyHookAction(data).then(() => {
      this.read(path);
    });
    data.message.envelope.author = data.message.envelope.author.substr(1);
    this.addPendingMessage(data.message.path, data.message.envelope);
    return promise;
  }

  /**
   * Mark chat as read
   */
  read(path: Path): Promise<any> {
    return this.storeAction({ read: { path } });
  }


  /**
   * Create a chat and setup metadata
   */
  create(
    title: string, description: string, appPath: string, groupPath: string,
    policy: any, members: PatpNoSig[], allowHistory: boolean, managed: boolean
  ): Promise<any> {
    return this.viewAction({
      create: {
        title,
        description,
        'app-path': appPath,
        'group-path': groupPath,
        policy,
        members,
        'allow-history': allowHistory,
        managed
      }
    });
  }

  /**
   * Deletes a chat
   *
   * If we don't host the chat, then it just leaves
   */
  delete(path: Path) {
    this.viewAction({ delete: { 'app-path': path } });
  }

  /**
   * Join a chat
   */
  join(ship: Patp, path: Path, askHistory: boolean): Promise<any> {
    return this.viewAction({
      join: {
        ship,
        'app-path': path,
        'ask-history': askHistory
      }
    });
  }

  /**
   * Groupify a chat that we host
   *
   * Will delete the old chat, recreate it based on a proper group,
   * and invite the current whitelist to that group.
   *  existing messages get moved over.
   *
   *  :existing is provided, associates chat with that group instead
   *  creating a new one. :inclusive indicates whether or not to add
   *  chat members to the group, if they aren't there already.
   */
  groupify(path: Path, group: Path | null = null, inclusive = false) {
    let action: any = { groupify: { 'app-path': path, existing: null } };
    if (group) {
      action.groupify.existing = {
        'group-path': group,
        inclusive: inclusive
      };
    }
    return this.viewAction(action);
  }

  /**
   * Begin syncing a chat from the host
   */
  addSynced(ship: Patp, path: Path, askHistory: boolean): Promise<any> {
    return this.action('chat-hook', 'chat-hook-action', {
      'add-synced': {
        ship,
        path,
        'ask-history': askHistory
      }
    });
  }

  invite(path: Path, ships: Patp[]) {
    return this.viewAction({ invite: { 'app-path': path, ships }})
  }


  private storeAction(action: ChatAction): Promise<any> {
    return this.action('chat-store', 'json', action)
  }

  private proxyHookAction(action: ChatAction): Promise<any> {
    return this.action('chat-hook', 'json', action);
  }

  private viewAction(action: unknown): Promise<any> {
    return this.action('chat-view', 'json', action);
  }

  private addPendingMessage(path: Path, envelope: Envelope) {
    const pending = this.store.state.pendingMessages.get(path);
    if (pending) {
      pending.unshift(envelope);
    } else {
      this.store.state.pendingMessages.set(path, [envelope]);
    }

    this.store.setState({
      pendingMessages: this.store.state.pendingMessages
    });
  }
}
