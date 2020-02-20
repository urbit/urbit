import React from 'react';
import ReactDOM from 'react-dom';
import _ from 'lodash';
import { uuid } from '/lib/util';
import { store } from '/store';


class UrbitApi {
  setAuthTokens(authTokens) {
    this.authTokens = authTokens;
    this.bindPaths = [];

    this.contactHook = {
      edit: this.contactEdit.bind(this),
      remove: this.contactRemove.bind(this)
    };

    this.contactView = {
      create: this.contactCreate.bind(this),
      delete: this.contactDelete.bind(this),
      share: this.contactShare.bind(this)
    };

    this.group = {
      add: this.groupAdd.bind(this)
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

  contactViewAction(data) {
    return this.action("contact-view", "json", data);
  }

  contactCreate(path, ships = []) {
    return this.contactViewAction({ create: { path, ships }});
  }

  groupAdd(path, ships = []) {
    return this.action("group-store", "group-action", {
      add: { members: ships, path }
    });
  }

  contactShare(recipient, path, ship, contact) {
    return this.contactViewAction({
      share: {
        recipient, path, ship, contact
      }
    });
  }

  contactDelete(path) {
    return this.contactViewAction({ delete: { path }});
  }

  contactHookAction(data) {
    return this.action("contact-hook", "contact-action", data);
  }

  contactRemove(path, ship) {
    return this.contactHookAction({
      remove: {
        path, ship
      }
    });
  }

  contactEdit(path, ship, editField) {
    /* editField can be...
    {nickname: ''}
    {email: ''}
    {phone: ''}
    {website: ''}
    {notes: ''}
    {color: 'fff'}  // with no 0x prefix
    {avatar: null}
    {avatar: {p: length, q: bytestream}}
    */
    return this.contactHookAction({
      edit: {
        path, ship, 'edit-field': editField
      }
    });
  }

  inviteAction(data) {
    return this.action("invite-store", "json", data);
  }

  inviteAccept(uid) {
    return this.inviteAction({
      accept: {
        path: '/contacts',
        uid
      }
    });
  }

  inviteDecline(uid) {
    return this.inviteAction({
      decline: {
        path: '/contacts',
        uid
      }
    });
  }

  setSpinner(boolean) {
    store.handleEvent({
      data: {
        local: {
          spinner: boolean
        }
      }
    })
  }

}

export let api = new UrbitApi();
window.api = api;
