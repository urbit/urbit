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
      unbundle: this.groupBundle.bind(this),
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
      removeOwned: this.inboxSyncRemoveOwned.bind(this),
      addSynced: this.inboxSyncAddSynced.bind(this),
      removeSynced: this.inboxSyncRemoveSynced.bind(this)
    };

  }

  bind(path, method, ship = this.authTokens.ship, app, success, fail) {
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
      (err) => {
        fail(err);
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

  groupsAction(data) {
    this.action("groups", "group-action", data);
  }

  groupBundle(path) {
    this.groupsAction({
      bundle: path
    });
  }

  groupUnbundle(path) {
    this.groupsAction({
      unbundle: path
    });
  }

  groupAdd(members, path) {
    this.groupsAction({
      add: {
        members,
        path
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

  inboxCreate(path, owner) {
    this.inboxAction({
      create: {
        path, owner
      }
    });
  }

  inboxDelete(path) {
    this.inboxAction({
      delete: {
        path
      }
    });
  }

  inboxMessage(local, path, author, when, letter) {
    let data = {
      message: {
        path,
        envelope: {
          uid: uuid(),
          author,
          when,
          letter
        }
      }
    };
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

  inboxSyncAction(data, mark = "sync-hook-action") {
    this.action("inbox-sync", mark, data);
  }

  inboxSyncAddOwned(path) {
    let data = {};
    data['add-owned'] = path;
    this.inboxSyncAction(data);
  }

  inboxSyncRemoveOwned(path) {
    let data = {};
    data['remove-owned'] = path;
    this.inboxSyncAction(data);
  }

  inboxSyncAddSynced(ship, path) {
    let data = {};
    data['add-synced'] = {
      ship,
      path
    };
    this.inboxSyncAction(data);
  }

  inboxSyncRemoveSynced(ship, path) {
    let data = {};
    data['remove-synced'] = {
      ship,
      path
    };
    this.inboxSyncAction(data);

  }

  permissionAction(data) {
    this.action("permissions", "permission-action", data);
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

  permissionCreate(path, kind, who) {
    this.permissionAction({
      create: {
        path,
        kind,
        who
      }
    });
  }

  permissionDelete(path) {
    this.permissionAction({
      delete: {
        path
      }
    });
  }

  permissionAdd(path, who) {
    this.permissionAction({
      add: {
        path,
        who
      }
    });
  }

  permissionRemove(path, who) {
    this.permissionAction({
      remove: {
        path,
        who
      }
    });
  }

  permissionAllow(path, who) {
    this.permissionAction({
      allow: {
        path,
        who
      }
    });
  }

  permissionDeny(path, who) {
    this.permissionAction({
      deny: {
        path,
        who
      }
    });
  }

}

export let api = new UrbitApi();
window.api = api;
