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

    this.inboxSync = {
      addOwned: this.inboxSyncAddOwned.bind(this),
      removeOwned: this.inboxSyncRemoveOwned.bind(this),
      addSynced: this.inboxSyncAddSynced.bind(this),
      removeSynced: this.inboxSyncRemoveSynced.bind(this)
    };

  }

  // keep default bind to hall, since its bind procedure more complex for now AA
  bind(path, method, ship = this.authTokens.ship, appl = "hall", success, fail) {
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    window.subscriptionId = window.urb.subscribe(ship, appl, path, 
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

  inboxMessage(local, path, author, when, message) {
    let data = {
      message: {
        path,
        envelope: {
          uid: uuid(),
          author,
          when,
          letter: {
            text: message
          }
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

}

export let api = new UrbitApi();
window.api = api;
