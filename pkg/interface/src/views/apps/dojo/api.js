import _ from 'lodash';

export default class Api {
  constructor(ship, channel) {
    this.ship = ship;
    this.channel = channel;
    this.bindPaths = [];
    this.dojoId = 'soto-' + Math.random().toString(36).substring(2);
  }

  bind(path, method, ship = this.ship, appl = 'dojo', success, fail) {
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

  soto(data) {
    return this.action('dojo', 'sole-action', { id: this.dojoId, dat: data });
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

