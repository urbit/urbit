if ('serviceWorker' in navigator && process.env.NODE_ENV !== 'development') {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/apps/landscape/serviceworker.js', {
      scope: '/apps/landscape'
    }).then((reg) => {
    });
  });
}
