import React from 'react';
import _ from 'lodash';
import { uuid } from '../../lib/util';
import store from './store';

class UrbitApi {
  setAuthTokens(authTokens, channel) {
    this.authTokens = authTokens;
    this.channel = channel;
    this.bindPaths = [];

    this.groups = {
      add: this.groupAdd.bind(this),
      remove: this.groupRemove.bind(this)
    };

    this.chat = {
      message: this.chatMessage.bind(this),
      read: this.chatRead.bind(this)
    };

    this.chatView = {
      create: this.chatViewCreate.bind(this),
      delete: this.chatViewDelete.bind(this),
      join: this.chatViewJoin.bind(this),
      groupify: this.chatViewGroupify.bind(this)
    };

    this.chatHook = {
      addSynced: this.chatHookAddSynced.bind(this)
    };

    this.invite = {
      accept: this.inviteAccept.bind(this),
      decline: this.inviteDecline.bind(this)
    };
  }

  bind(path, method, ship = this.authTokens.ship, app, success, fail, quit) {
    console.log(this.channel);
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    window.subscriptionId = this.channel.subscribe(ship, app, path,
      (err) => {
        fail(err);
      },
      (event) => {
        success({
          data: event,
          from: {
            ship,
            path
          }
        });
      },
      (qui) => {
        quit(qui);
      });
  }

  action(appl, mark, data) {
    return new Promise((resolve, reject) => {
      this.channel.poke(ship, appl, mark, data,
        (json) => {
          resolve(json);
        },
        (err) => {
          reject(err);
        });
    });
  }

  addPendingMessage(msg) {
    if (store.state.pendingMessages.has(msg.path)) {
      store.state.pendingMessages.get(msg.path).push(msg.envelope);
    } else {
      store.state.pendingMessages.set(msg.path, [msg.envelope]);
    }

    store.setState({
      pendingMessages: store.state.pendingMessages
    });
  }

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

  sidebarToggle() {
    let sidebarBoolean = true;
    if (store.state.sidebarShown === true) {
      sidebarBoolean = false;
    }
    store.handleEvent({
      data: {
        local: {
          'sidebarToggle': sidebarBoolean
        }
      }
    });
  }

  setSelected(selected) {
    store.handleEvent({
      data: {
        local: {
          selected: selected
        }
      }
    });
  }
}

const api = new UrbitApi();
export default api;
