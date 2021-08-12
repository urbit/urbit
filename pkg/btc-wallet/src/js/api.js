import _ from 'lodash';

class UrbitApi {
  setChannel(ship, channel) {
    this.ship = ship;
    this.channel = channel;
    this.bindPaths = [];
  }

  bind(path, method, ship = this.ship, appl = 'btc-wallet', success, fail) {
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    window.subscriptionId = this.channel.subscribe(
      ship,
      appl,
      path,
      (err) => {
        fail(err);
      },
      (event) => {
        success({
          data: event,
          from: {
            ship,
            path,
          },
        });
      },
      (err) => {
        fail(err);
      }
    );
  }

  btcWalletCommand(data) {
    return this.action('btc-wallet', 'btc-wallet-command', data);
  }

  settingsEvent(data) {
    return this.action('settings-store', 'settings-event', data);
  }

  action(appl, mark, data) {
    return new Promise((resolve, reject) => {
      this.channel.poke(
        this.ship,
        appl,
        mark,
        data,
        (json) => {
          resolve(json);
        },
        (err) => {
          reject(err);
        }
      );
    });
  }
}
export let api = new UrbitApi();
window.api = api;
