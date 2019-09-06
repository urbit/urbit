const { ipcRenderer } = require('electron');

console.log("renderer.js loaded");

const redirectToShip = (port) => {
    const url = `http://localhost:${port}`;
    console.log(url);
    document.write("<h1>Redirecting</h1>");
    window.setTimeout(() => { window.location = url; }, 2000);
};

ipcRenderer.send('renderer-started', null);
ipcRenderer.on('urbit-started', (event, port) => redirectToShip(port));
