if ('serviceWorker' in navigator && process.env.NODE_ENV !== 'development') {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/~landscape/js/bundle/serviceworker.js', {
      scope: '/'
    }).then((reg) => {
    });
  });
}
