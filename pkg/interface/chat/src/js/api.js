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

  groups(data) {
    this.action("groups", "group-action", data);
  }

  bundle(path) {
    this.groups({
      bundle: path
    });
  }

  unbundle(path) {
    this.groups({
      unbundle: path
    });
  }

  add(members, path) {
    this.groups({
      add: {
        members,
        path
      }
    });
  }

  remove(members, path) {
    this.groups({
      remove: {
        members, path
      }
    });
  }


}

export let api = new UrbitApi();
window.api = api;
