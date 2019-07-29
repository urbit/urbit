
class Api {
  bind(app, path, success, fail, ship) {
    window.urb.subscribe(ship, app, path, 
      (err) => {
        fail(err, app, path, ship);
      },
      (event) => {
        success({
          data: event,
          from: {
            app,
            ship,
            path
          }
        });
      },
      (err) => {
        fail(err, app, path, ship);
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
}

export let api = new Api();
window.api = api;
