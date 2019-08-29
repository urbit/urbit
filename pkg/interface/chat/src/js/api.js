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

  inboxMessage(path, author, when, message) {
    this.inboxAction({
      message: {
        path,
        envelope: {
          author,
          when,
          message
        }
      }
    });
  }

  inboxRead(path, read) {
    this.inboxAction({
      read: {
        path, read
      }
    });
  }


}

export let api = new UrbitApi();
window.api = api;
