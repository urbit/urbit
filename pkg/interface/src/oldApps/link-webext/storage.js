// use synced storage if supported, fall back to local
const storage = browser.storage.sync || browser.storage.local;

const setEndpoint = (endpoint) => {
  return storage.set({endpoint});
}

const getEndpoint = () => {
  return new Promise((resolve, reject) => {
    storage.get("endpoint").then((res) => {
      if (res && res.endpoint) {
        resolve(res.endpoint);
      } else {
        resolve(null);
      }
    }, (err) => {
      resolve(null);
    });
  });
}
