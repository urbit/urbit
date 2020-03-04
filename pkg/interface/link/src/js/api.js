import React from 'react';
import ReactDOM from 'react-dom';
import _ from 'lodash';
import { uuid, stringToTa } from '/lib/util';
import { store } from '/store';
import moment from 'moment';


class UrbitApi {
  setAuthTokens(authTokens) {
    this.authTokens = authTokens;
    this.bindPaths = [];

    this.invite = {
      accept: this.inviteAccept.bind(this),
      decline: this.inviteDecline.bind(this),
      invite: this.inviteInvite.bind(this)
    };

    this.bind = this.bind.bind(this);
    this.bindLinkView = this.bindLinkView.bind(this);
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

  bindLinkView(path, result, fail, quit) {
    this.bind.bind(this)(
      path, 'PUT', this.authTokens.ship, 'link-view',
      result, fail, quit
    );
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

  getCommentsPage(path, url, page) {
    const strictUrl = stringToTa(url);
    const endpoint = '/json/' + page + '/discussions/' + strictUrl + path;
    this.bindLinkView(endpoint,
      (res) => {
        if (res.data['initial-discussions']) {
          // these aren't returned with the response,
          // so this ensures the reducers know them.
          res.data['initial-discussions'].path = path;
          res.data['initial-discussions'].url = url;
        }
        store.handleEvent(res);
      },
      console.error,
      ()=>{} // no-op on quit
    );
  }

  getPage(path, page) {
    const endpoint = '/json/' + page + '/submissions' + path;
    this.bindLinkView(endpoint,
      (dat)=>{store.handleEvent(dat)},
      console.error,
      ()=>{} // no-op on quit
    );
  }

  getSubmission(path, url, callback) {
    const strictUrl = stringToTa(url);
    const endpoint = '/json/0/submission/' + strictUrl + path;
    this.bindLinkView(endpoint,
      (res) => {
        if (res.data.submission) {
          callback(res.data.submission)
        } else {
          console.error('unexpected submission response', res);
        }
      },
      console.error,
      ()=>{} // no-op on quit
    );
  }

  linkAction(data) {
    return this.action("link-store", "link-action", data);
  }

  postLink(path, url, title) {
    return this.linkAction({
      'save': { path, url, title }
    });
  }

  postComment(path, url, comment) {
    return this.linkAction({
      'note': { path, url, udon: comment }
    });
  }

  // leave url as null to mark all under path as read
  seenLink(path, url = null) {
    return this.linkAction({
      'seen': { path, url }
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
