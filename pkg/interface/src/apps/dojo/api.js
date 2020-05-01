import _ from 'lodash';

class UrbitApi {
  setAuthTokens(authTokens, channel) {
    this.authTokens = authTokens;
    this.channel = channel;
    this.bindPaths = [];
  }

  bind(path, method, ship = this.authTokens.ship, appl = 'dojo', success, fail) {
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
    return this.action('dojo', 'sole-action',
      { id: this.authTokens.dojoId, dat: data }
    );
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

const api = new UrbitApi();
export default api;
