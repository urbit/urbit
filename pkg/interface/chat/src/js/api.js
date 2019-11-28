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
      decline: this.inviteDecline.bind(this),
      invite: this.inviteInvite.bind(this)
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
    this.action("chat-store", "json", data);
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
    this.addPendingMessage(data.message);
  }

  chatRead(path, read) {
    this.chatAction({ read: { path } });
  }

  chatViewAction(data) {
    this.action("chat-view", "json", data);
  }

  chatViewCreate(path, security, read, write, allowHistory) {
    this.chatViewAction({
      create: {
        path, security, read, write,
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
  
  inviteInvite(path, ship) {
    this.action("invite-hook", "json", 
      {
        invite: {
          path: '/chat',
          invite: {
            path,
            ship: `~${window.ship}`,
            recipient: ship,
            app: 'chat-hook',
            text: `~${window.ship}${path}`,
          },
          uid: uuid()
        }
      }
    );
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

  storeState(state) {
    store.setState(state);
  }

}

export let api = new UrbitApi();
window.api = api;
