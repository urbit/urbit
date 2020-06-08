import BaseApi from './base';
import { uuid } from '../lib/util';

export default class ChatApi {
  constructor(ship, channel, store) {
    const helper = new PrivateHelper(ship, channel, store);

    this.ship = ship;
    this.subscribe = helper.subscribe.bind(helper);

    this.groups = {
      add: helper.groupAdd.bind(helper),
      remove: helper.groupRemove.bind(helper)
    };

    this.chat = {
      message: helper.chatMessage.bind(helper),
      read: helper.chatRead.bind(helper)
    };

    this.chatView = {
      create: helper.chatViewCreate.bind(helper),
      delete: helper.chatViewDelete.bind(helper),
      join: helper.chatViewJoin.bind(helper),
      groupify: helper.chatViewGroupify.bind(helper)
    };

    this.chatHook = {
      addSynced: helper.chatHookAddSynced.bind(helper)
    };

    this.invite = {
      accept: helper.inviteAccept.bind(helper),
      decline: helper.inviteDecline.bind(helper)
    };

    this.metadata = {
      add: helper.metadataAdd.bind(helper)
    };
  }
}

class PrivateHelper extends BaseApi {
  groupsAction(data) {
    return this.action('group-store', 'group-action', data);
  }

  groupAdd(members, path) {
    return this.groupsAction({
      add: {
        members, path
      }
    });
  }

  groupRemove(members, path) {
    this.groupsAction({
      remove: {
        members, path
      }
    });
  }

  chatAction(data) {
    this.action('chat-store', 'json', data);
  }

  addPendingMessage(msg) {
    if (this.store.state.pendingMessages.has(msg.path)) {
      this.store.state.pendingMessages.get(msg.path).unshift(msg.envelope);
    } else {
      this.store.state.pendingMessages.set(msg.path, [msg.envelope]);
    }

    this.store.setState({
      pendingMessages: this.store.state.pendingMessages
    });
  }

  chatMessage(path, author, when, letter) {
    const data = {
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

    this.action('chat-hook', 'json', data).then(() => {
      this.chatRead(path);
    });
    data.message.envelope.author = data.message.envelope.author.substr(1);
    this.addPendingMessage(data.message);
  }

  chatRead(path, read) {
    this.chatAction({ read: { path } });
  }

  chatHookAddSynced(ship, path, askHistory) {
    return this.action('chat-hook', 'chat-hook-action', {
      'add-synced': {
        ship,
        path,
        'ask-history': askHistory
      }
    });
  }

  chatViewAction(data) {
    return this.action('chat-view', 'json', data);
  }

  chatViewCreate(
    title, description, appPath, groupPath,
    security, members, allowHistory
  ) {
    return this.chatViewAction({
      create: {
        title,
        description,
        'app-path': appPath,
        'group-path': groupPath,
        security,
        members,
        'allow-history': allowHistory
      }
    });
  }

  chatViewDelete(path) {
    this.chatViewAction({ delete: { 'app-path': path } });
  }

  chatViewJoin(ship, path, askHistory) {
    this.chatViewAction({
      join: {
        ship,
        'app-path': path,
        'ask-history': askHistory
      }
    });
  }

  chatViewGroupify(path, group = null, inclusive = false) {
    const action = { groupify: { 'app-path': path, existing: null } };
    if (group) {
      action.groupify.existing = {
        'group-path': group,
        inclusive: inclusive
      };
    }
    return this.chatViewAction(action);
  }

  inviteAction(data) {
    this.action('invite-store', 'json', data);
  }

  inviteAccept(uid) {
    this.inviteAction({
      accept: {
        path: '/chat',
        uid
      }
    });
  }

  inviteDecline(uid) {
    this.inviteAction({
      decline: {
        path: '/chat',
        uid
      }
    });
  }

  metadataAction(data) {
    return this.action('metadata-hook', 'metadata-action', data);
  }

  metadataAdd(appPath, groupPath, title, description, dateCreated, color) {
    const creator = `~${window.ship}`;
    return this.metadataAction({
      add: {
        'group-path': groupPath,
        resource: {
          'app-path': appPath,
          'app-name': 'chat'
        },
        metadata: {
          title,
          description,
          color,
          'date-created': dateCreated,
          creator
        }
      }
    });
  }

}

