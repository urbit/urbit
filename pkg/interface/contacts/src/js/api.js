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
      edit: this.contactEdit.bind(this)
    };

    this.contactView = {
      create: this.contactCreate.bind(this),
      delete: this.contactDelete.bind(this),
      add: this.contactAdd.bind(this),
      remove: this.contactRemove.bind(this),
      share: this.contactShare.bind(this)
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
    this.action("contact-view", "json", data);
  }

  contactCreate(path, ships = []) {
    this.contactViewAction({ create: { path, ships }});
  }

  contactAdd(path, ships = []) {
    this.contactViewAction({ add: { path, ships }});
  }

  contactShare(recipient, path, ship, contact) {
    this.contactViewAction({
      share: {
        recipient, path, ship, contact
      }
    });
  }

  contactDelete(path) {
    this.contactViewAction({ delete: { path }});
  }

  contactRemove(path, ship) {
    this.contactViewAction({
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
    this.action("contact-hook", "contact-action", {
      edit: {
        path, ship, 'edit-field': editField
      }
    });
  }

  inviteAction(data) {
    this.action("invite-store", "json", data);
  }
  
  inviteAccept(uid) {
    this.inviteAction({
      accept: {
        path: '/contacts',
        uid
      }
    });
  }
  
  inviteDecline(uid) {
    this.inviteAction({
      decline: {
        path: '/contacts',
        uid
      }
    });
  }

}

export let api = new UrbitApi();
window.api = api;
