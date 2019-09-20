import React from 'react';
import ReactDOM from 'react-dom';
import _ from 'lodash';
import { uuid } from '/lib/util';
import { store } from '/store';


class UrbitApi {
  setAuthTokens(authTokens) {
    this.authTokens = authTokens;
    this.bindPaths = [];
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

  hall(data) {
    this.action("hall", "json", data);
  }
  
  addPendingMessage(data) {
    let pendingMap = store.state.pendingMessages;
    if (pendingMap.has(data.aud[0])) {
      pendingMap.get(data.aud[0]).push(data)     
    } else {
      pendingMap.set(data.aud[0], [data])
    }
    store.setState({
      pendingMessages: pendingMap
    });
  }

  chat(lis) {
    this.action("chat", "chat-action", {
      actions: {
        lis
      }
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

  notify(aud, bool) {
    this.hall({
      notify: {
        aud,
        pes: !!bool ? 'hear' : 'gone'
      }
    });
  }

  permit(cir, aud, message) {
    this.hall({
      permit: {
        nom: cir,
        sis: aud,
        inv: true
      }
    });

    if (message) {
      this.invite(cir, aud);
    }
  }

  unpermit(cir, ship) {
    /*
     * lol, never send an unpermit to yourself.
     * it puts your ship into an infinite loop.
     * */
    if (ship === window.ship) {
      return;
    }
    this.hall({
      permit: {
        nom: cir,
        sis: [ship],
        inv: false
      }
    });
  }

  invite(cir, aud) {
    let audInboxes = aud.map((aud) => `~${aud}/i`);
    let inviteMessage = {
      aud: audInboxes,
      ses: [{
        inv: {
          inv: true,
          cir: `~${window.ship}/${cir}`
        }
      }]
    };

    this.hall({
      phrase: inviteMessage
    });
  }

  source(nom, sub) {
    this.hall({
      source: {
        nom: "inbox",
        sub: sub,
        srs: [nom]
      }
    })
  }

  delete(nom) {
    this.hall({
      delete: {
        nom,
        why: ''
      }
    })
  }

  read(nom, red) {
    this.hall({
      read: {
        nom,
        red
      }
    })
  }
}

export let api = new UrbitApi();
window.api = api;
