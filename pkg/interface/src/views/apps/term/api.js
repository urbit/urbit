import _ from 'lodash';

export default class Api {
  constructor(ship, channel) {
    this.ship = ship;
    this.channel = channel;
    this.bindPaths = [];
  }

  bind(path, method, ship = this.ship, appl = 'herm', success, fail) {
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    window.subscriptionId = this.channel.subscribe(ship, appl, path,
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

  belt(belt) {
    return this.action('herm', 'belt', belt);
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
}

