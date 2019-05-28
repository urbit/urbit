import React from 'react';
import ReactDOM from 'react-dom';
import _ from 'lodash';
import { uuid } from '/lib/util';

class UrbitApi {
  setAuthTokens(authTokens) {
    this.authTokens = authTokens;
    this.bindPaths = [];
  }

  // keep default bind to hall, since its bind procedure more complex for now AA
  bind(path, method, ship = this.authTokens.ship, appl = "hall", success, fail) {
    console.log('binding to ...', appl, ", path: ", path, ", as ship: ", ship, ", by method: ", method);
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    window.urb.subscribe(ship, appl, path, 
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
    this.action("hall", "hall-action", data);
  }

  coll(data) {
    this.action("collections", "collections-action", data);
  }

  setOnboardingBit(value) {
    this.action("collections", "json", { onboard: value });
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

  /*
    Special actions
  */

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

  permitCol(cir, aud, message, nom) {
    this.hall({
      permit: {
        nom: cir,
        sis: aud,
        inv: true
      }
    });

    if (message) {
      this.inviteCol(cir, aud, nom);
    }
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

  inviteCol(cir, aud, nom) {
    let audInboxes = aud.map((aud) => `~${aud}/i`);
    let inviteMessage = {
      aud: audInboxes,
      ses: [{
        app: {
          app: nom,
          sep: {
            inv: {
              inv: true,
              cir: `~${window.ship}/${cir}`
            }
          }
        }
      }]
    };

    this.hall({
      phrase: inviteMessage
    });
  }

  message(aud, words) {
    let msg = {
      aud,
      ses: [{
        lin: {
          msg: words,
          pat: false
        }
      }]
    };

    this.hall({
      phrase: msg
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

  create(nom, priv) {
    this.hall({
      create: {
        nom: nom,
        des: "chatroom",
        sec: priv ? "village" : "channel"
      }
    });
  }

  ire(aud, uid, msg) {
    let message = {
      aud: aud,
      ses: [{
        ire: {
          top: uid,
          sep: {
            lin: {
              msg: msg,
              pat: false
            }
          }
        }
      }]
    }

    this.hall({
      phrase: message
    })
  }
}

export let api = new UrbitApi();
window.api = api;
