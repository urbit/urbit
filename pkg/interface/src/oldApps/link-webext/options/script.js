
function storeOptions(e) {
  e.preventDefault();

  // clean up endpoint address and store it
  let endpoint = document.querySelector("#endpoint").value
    .replace(/^.*:\/\//, '') // strip protocol
    .replace(/\/+$/, ''); // strip trailing slashes
  setEndpoint(endpoint);
}

async function restoreOptions() {

  const endpoint = await getEndpoint();
  console.log('prefilling with', endpoint);

  document.querySelector("#endpoint").value = endpoint;
}

document.addEventListener("DOMContentLoaded", restoreOptions);
document.querySelector("form").addEventListener("submit", storeOptions);