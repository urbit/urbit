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
      bundle: this.groupBundle.bind(this),
      unbundle: this.groupUnbundle.bind(this),
      add: this.groupAdd.bind(this),
      remove: this.groupRemove.bind(this)
    };
    
    this.chat = {
      create: this.chatCreate.bind(this),
      delete: this.chatDelete.bind(this),
      message: this.chatMessage.bind(this),
      read: this.chatRead.bind(this)
    };

    this.chatView = {
      create: this.chatViewCreate.bind(this),
      delete: this.chatViewDelete.bind(this),
    };

    this.invite = {
      create: this.inviteCreate.bind(this),
      delete: this.inviteDelete.bind(this),
      accept: this.inviteAccept.bind(this),
      decline: this.inviteDecline.bind(this),
      invite: this.inviteInvite.bind(this)
    };

    this.permissions = {
      create: this.permissionCreate.bind(this),
      delete: this.permissionDelete.bind(this),
      add: this.permissionAdd.bind(this),
      remove: this.permissionRemove.bind(this)
    };

    this.chatHook = {
      addOwned: this.chatHookAddOwned.bind(this),
      addSynced: this.chatHookAddSynced.bind(this),
      remove: this.chatHookRemove.bind(this)
    };

    this.permissionGroupHook = {
      associate: this.permissionGroupHookAssociate.bind(this),
      dissociate: this.permissionGroupHookDissociate.bind(this)
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

  groupBundle(path) {
    this.groupsAction({ bundle: path });
  }

  groupUnbundle(path) {
    this.groupsAction({ unbundle: path });
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

  chatCreate(path, owner = `~${window.ship}`) {
    this.chatAction({
      create: {
        path, owner
      }
    });
  }

  chatDelete(path) {
    this.chatAction({ delete: { path } });
  }

  chatMessage(local, path, author, when, letter) {
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

    this.addPendingMessage(data.message);
    if (local) {
      this.chatAction(data);
    } else {
      this.chatHookAction(data, "json");
    }
  }

  chatRead(path, read) {
    this.chatAction({ read: { path } });
  }

  chatViewAction(data) {
    console.log(data);
    this.action("chat-view", "json", data);
  }

  chatViewCreate(path, security, read, write) {
    this.chatViewAction({
      create: {
        path, security, read, write
      }
    });
  }

  chatViewDelete(path) {
    this.chatViewAction({ delete: { path } });
  }

  inviteAction(data) {
    console.log(data);
    this.action("invite-store", "json", data);
  }

  inviteCreate(path) {
    this.inviteAction({ create: { path } });
  }

  inviteDelete(path) {
    this.inviteAction({ delete: { path } });
  }

  inviteAccept(path, uid) {
    this.inviteAction({ accept: { path, uid } });
  }

  inviteDecline(path, uid) {
    this.inviteAction({ decline: { path, uid } });
  }

  inviteInvite(path, peerPath, ship, app, text, recipient) {
    this.inviteAction({
      invite: {
        path, 
        invite: {
          'peer-path': peerPath,
          dock: { ship, app },
          text, recipient
        },
        uid: uuid()
      }
    });
  }

  chatHookAction(data, mark = "chat-hook-action") {
    this.action("chat-hook", mark, data);
  }

  chatHookAddOwned(path, security) {
    let data = {};
    data['add-owned'] = {
      path, security
    };
    this.chatHookAction(data);
  }

  chatHookRemove(path) {
    this.chatHookAction({ remove: path });
  }

  chatHookAddSynced(ship, path) {
    let data = {};
    data['add-synced'] = {
      ship, path
    };
    this.chatHookAction(data);
  }

  permissionAction(data) {
    this.action("permission-store", "permission-action", data);
  }

  permissionCreate(path, kind, who) {
    this.permissionAction({
      create: {
        path, kind, who
      }
    });
  }

  permissionDelete(path) {
    this.permissionAction({ delete: { path } });
  }

  permissionAdd(path, who) {
    this.permissionAction({
      add: {
        path, who
      }
    });
  }

  permissionRemove(path, who) {
    this.permissionAction({
      remove: {
        path, who
      }
    });
  }

  permissionAllow(path, who) {
    this.permissionAction({
      allow: {
        path, who
      }
    });
  }

  permissionDeny(path, who) {
    this.permissionAction({
      deny: {
        path, who
      }
    });
  }

  permissionGroupHookAction(data) {
    this.action("permission-group-hook", "permission-group-hook-action", data);
  }
  
  permissionGroupHookAssociate(group, permissions) {
    this.permissionGroupHookAction({
      associate: {
        group, permissions
      }
    });
  }
  
  permissionGroupHookDissociate() {
    this.permissionGroupHookAction({
      dissociate: {
        group, permissions
      }
    });
  }

}

export let api = new UrbitApi();
window.api = api;
