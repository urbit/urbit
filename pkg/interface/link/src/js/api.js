import React from 'react';
import ReactDOM from 'react-dom';
import _ from 'lodash';
import { uuid } from '/lib/util';
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
    const strictUrl = this.encodeUrl(url);
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
    const strictUrl = this.encodeUrl(url);
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

  //TODO into lib?
  // encode the url into @ta-safe format, using logic from +wood
  encodeUrl(url) {
    let strictUrl = '';
    for (let i = 0; i < url.length; i++) {
      const char = url[i];
      let add = '';
      switch (char) {
        case ' ':
          add = '.';
          break;
        case '.':
          add = '~.';
          break;
        case '~':
          add = '~~';
          break;
        default:
          const charCode = url.charCodeAt(i);
          if (
            (charCode >= 97 && charCode <= 122) || // a-z
            (charCode >= 48 && charCode <= 57)  || // 0-9
            char === '-'
          ) {
            add = char;
          } else {
            //TODO behavior for unicode doesn't match +wood's,
            //     but we can probably get away with that for now.
            add = '~' + charCode.toString(16) + '.';
          }
      }
      strictUrl = strictUrl + add;
    }
    return '~.' + strictUrl;
  }

}

export let api = new UrbitApi();
window.api = api;
