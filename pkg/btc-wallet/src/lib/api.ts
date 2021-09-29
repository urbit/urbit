import _ from 'lodash';

type Channel = {
  poke: (
    ship: string,
    appl: string,
    mark: string,
    data: any,
    postDataHandler: (json: any) => void,
    errorHandler: (err: string) => void
  ) => void;
  subscribe: (
    ship: string,
    appl: string,
    path: string,
    errorHandler: (err: string) => void,
    eventHandler: (event: any) => void
  ) => void;
};

class UrbitApi {
  ship: string;
  channel: Channel;
  bindPaths: string[];
  setChannel(ship: string, channel: Channel) {
    this.ship = ship;
    this.channel = channel;
    this.bindPaths = [];
  }

  bind(
    path: string,
    method: string,
    ship = this.ship,
    appl = 'btc-wallet',
    success: any,
    fail: any
  ) {
    this.bindPaths = _.uniq([...this.bindPaths, path]);

    (window as any).subscriptionId = this.channel.subscribe(
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
      }
    );
  }

  btcWalletCommand(data: any) {
    return this.action('btc-wallet', 'btc-wallet-command', data);
  }

  settingsEvent(data: any) {
    return this.action('settings-store', 'settings-event', data);
  }

  action(appl: string, mark: string, data: string) {
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
(window as any).api = api;
