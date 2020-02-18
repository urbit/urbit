import React from 'react';
import ReactDOM from 'react-dom';
import _ from 'lodash';
import { uuid } from '/lib/util';
import { store } from '/store';


class UrbitApi {
  setAuthTokens(authTokens) {
    this.authTokens = authTokens;
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
    };

    this.invite = {
      accept: this.inviteAccept.bind(this),
      decline: this.inviteDecline.bind(this)
    };
  }

  bind(path, method, ship = this.authTokens.ship, app, success, fail, quit) {
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    window.subscriptionId = window.urb.subscribe(ship, app, path,
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
      window.urb.poke(ship, appl, mark, data,
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

  addReadReceiptPending(station) {
    const m = store.state.myReadReceiptsPending.set(station, true);
    store.setState({myReadReceiptsPending: m})
  }

  removeReadReceiptPending(station) {
    const m = store.state.myReadReceiptsPending.set(station, false);
    store.setState({myReadReceiptsPending: m})
  }

  groupsAction(data) {
    this.action("group-store", "group-action", data);
  }

  groupAdd(members, path) {
    this.groupsAction({
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
    return this.action("chat-store", "json", data);
  }

  chatMessage(path, author, when, letter) {
    let data = {
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

    this.action("chat-hook", "json", data);
    data.message.envelope.author = data.message.envelope.author.substr(1);
    this.addPendingMessage(data.message);
  }

  chatRead(path, read) {
    return this.chatAction({ read: { path } });
  }

  chatViewAction(data) {
    this.action("chat-view", "json", data);
  }

  chatViewCreate(path, security, members, allowHistory) {
    this.chatViewAction({
      create: {
        path, security, members,
        'allow-history': allowHistory
      }
    });
  }

  chatViewDelete(path) {
    this.chatViewAction({ delete: { path } });
  }

  chatViewJoin(ship, path, askHistory) {
    this.chatViewAction({
      join: {
        ship, path,
        'ask-history': askHistory
      }
    });
  }

  inviteAction(data) {
    this.action("invite-store", "json", data);
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
}

export let api = new UrbitApi();
window.api = api;
