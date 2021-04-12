

if ("serviceWorker" in navigator) {
  window.addEventListener("load", () => {
    navigator.serviceWorker.register("/~landscape/js/bundle/serviceworker.js", {
      scope: "/",
    }).then(reg => {
    });
  });
}
