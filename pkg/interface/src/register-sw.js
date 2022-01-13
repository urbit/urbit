if ('serviceWorker' in navigator && process.env.NODE_ENV !== 'development') {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/apps/escape/serviceworker.js', {
      scope: '/apps/escape/'
    }).then((reg) => {
    });
  });
}
