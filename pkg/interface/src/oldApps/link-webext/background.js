
const attemptPost = (endpoint, path, data) => {
  console.log('sending', data, JSON.stringify(data));
  return new Promise((resolve, reject) => {
    fetch(`http://${endpoint}/~link${path}`, {
      method: 'POST',
      credentials: 'include',
      body: JSON.stringify(data)
    })
    .then(response => {
      console.log('resp', response.status);
      resolve(response.status === 200);
    })
    .catch(error => {
      console.error('post failed', error);
      resolve(false);
    });
  });
}

const attemptGet = (endpoint, path, data) => {
  return new Promise((resolve, reject) => {
    fetch(`http://${endpoint}/~link{path}`, {
      method: 'GET',
      credentials: 'include',
      body: JSON.stringify(data)
    })
    .then(response => {
      console.log('get response');
      console.log('response', response);
      resolve(true);
    })
    .catch(error => {
      console.log('fetch error', error);
      resolve(false);
    });
  });
}

const saveUrl = (endpoint, title, url) => {
  return attemptPost(endpoint, '/add/private', {title, url});
}

const openOptions = () => {
  browser.tabs.create({
    url: browser.runtime.getURL('options/index.html')
  });
}

const openLogin = (endpoint) => {
  browser.tabs.create({
    url: `http://${endpoint}/~/login`
  });
}

const doSave = async () => {
  console.log('gonna do save!');
  // if no endpoint, refer to options page
  const endpoint = await getEndpoint();
  console.log('endpoint', endpoint);
  if (endpoint === null) {
    return openOptions();
  }

  const tab = (await browser.tabs.query({currentWindow: true, active: true}))[0];
  //TODO figure out if we're viewing urbit page, turn into arvo:// url?
  const success = await saveUrl(endpoint, tab.title, tab.url);
  console.log('success', success);
  if (!success) {
    console.log('failed, opening login');
    openLogin(endpoint);
  } else {
    console.log('success!');
  }
}


//  perform save action when extension button is clicked
//TODO want to do a pop-up instead of on-click action here latern
//
browser.browserAction.onClicked.addListener(doSave);

//  open settings page on-install, user will need to set endpoint
//
browser.runtime.onInstalled.addListener(async ({ reason, temporary }) => {
  // if (temporary) return; // skip during development
  switch (reason) {
    case "install":
      browser.runtime.openOptionsPage();
    break;
  }
});
