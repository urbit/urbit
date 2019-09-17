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
    
    this.inbox = {
      create: this.inboxCreate.bind(this),
      delete: this.inboxDelete.bind(this),
      message: this.inboxMessage.bind(this),
      read: this.inboxRead.bind(this)
    };

    this.permissions = {
      create: this.permissionCreate.bind(this),
      delete: this.permissionDelete.bind(this),
      add: this.permissionAdd.bind(this),
      remove: this.permissionRemove.bind(this)
    };

    this.inboxSync = {
      addOwned: this.inboxSyncAddOwned.bind(this),
      addSynced: this.inboxSyncAddSynced.bind(this),
      remove: this.inboxSyncRemove.bind(this)
    };

    this.groupPermit = {
      associate: this.groupPermitAssociate.bind(this),
      dissociate: this.groupPermitDissociate.bind(this)
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
    this.action("groups", "group-action", data);
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

  inboxAction(data) {
    this.action("inbox", "inbox-action", data);
  }

  inboxCreate(path, owner = `~${window.ship}`) {
    this.inboxAction({
      create: {
        path, owner
      }
    });
  }

  inboxDelete(path) {
    this.inboxAction({ delete: { path } });
  }

  inboxMessage(local, path, author, when, letter) {
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
      this.inboxAction(data);
    } else {
      this.inboxSyncAction(data, "inbox-action");
    }
  }

  inboxRead(path, read) {
    this.inboxAction({
      read: {
        path, read
      }
    });
  }

  inboxSyncAction(data, mark = "permission-hook-action") {
    this.action("inbox-sync", mark, data);
  }

  inboxSyncAddOwned(path, security) {
    let data = {};
    data['add-owned'] = {
      path, security
    };
    this.inboxSyncAction(data);
  }

  inboxSyncRemove(path) {
    this.inboxSyncAction({ remove: path });
  }

  inboxSyncAddSynced(ship, path) {
    let data = {};
    data['add-synced'] = {
      ship, path
    };
    this.inboxSyncAction(data);
  }

  permissionAction(data) {
    this.action("permissions", "permission-action", data);
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

  groupPermitAction(data) {
    this.action("group-permit", "group-permit-action", data);
  }
  
  groupPermitAssociate(group, permissions) {
    this.groupPermitAction({
      associate: {
        group, permissions
      }
    });
  }
  
  groupPermitDissociate() {
    this.groupPermitAction({
      dissociate: {
        group, permissions
      }
    });
  }

}

export let api = new UrbitApi();
window.api = api;
