let iframe = document.createElement('iframe');
iframe.setAttribute('src', window.state.url);
iframe.setAttribute('width', '100%;');
iframe.setAttribute('height', '100%;');
iframe.setAttribute('style', 'border-style: none !important;');
document.body.appendChild(iframe);
