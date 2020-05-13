import _ from 'lodash';
export default class Api {
  constructor(ship, channel, store) {
    this.ship = ship;
    this.channel = channel;
    this.store = store;
    this.bindPaths = [];
  }

  bind(path, method, ship = this.ship, app, success, fail, quit) {
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    window.subscriptionId = this.channel.subscribe(ship, app, path,
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
      this.channel.poke(window.ship, appl, mark, data,
        (json) => {
          resolve(json);
        },
        (err) => {
          reject(err);
        });
    });
  }

  setSelected(selected) {
    this.store.handleEvent({
      data: {
        local: {
          selected: selected
        }
      }
    });
  }
}
