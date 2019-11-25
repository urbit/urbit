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
    
    this.contacts = {
      create: this.contactCreate.bind(this),
      delete: this.contactDelete.bind(this),
      add: this.contactAdd.bind(this),
      remove: this.contactRemove.bind(this),
      edit: this.contactEdit.bind(this)
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

  contactAction(data) {
    this.action("contact-store", "json", data);
  }

  contactCreate(path) {
    this.contactAction({ create: { path }});
  }

  contactDelete(path) {
    this.contactAction({ delete: { path }});
  }

  contactAdd(path, ship, contact = {
    nickname: '',
    email: '',
    phone: '',
    website: '',
    notes: '',
    color: '0x000000',
    avatar: null
  }) {
    this.contactAction({
      add: {
        path, ship, contact
      }
    });
  }

  contactRemove(path, ship) {
    this.contactAction({
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
    {color: '0xfff'}
    {avatar: null}
    {avatar: {p: length, q: bytestream}}
    */
    this.contactAction({
      edit: {
        path, ship, 'edit-field': editField
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
            text: `You have been invited to /${window.ship}${path}`,
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

}

export let api = new UrbitApi();
window.api = api;
