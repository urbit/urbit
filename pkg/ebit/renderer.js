const { ipcRenderer } = require('electron');

console.log("renderer.js loaded");

const redirectToShip = (port) => {
    const url = `http://localhost:${port}`;
    console.log(url);
    document.write("<h1>Redirecting</h1>");
    window.setTimeout(() => { window.location = url; }, 2000);
};

const ships = ipcRenderer.sendSync('ship-list');

var body = '';

body += '<ul>';

const doship = (ship) => {
  document.body.innerHTML = `Starting ${ship}`;
  ipcRenderer.send('start-ship', ship);
}

const mkship = () => {
  const ship = document.getElementById("bootname").value;
  document.body.innerHTML = `Booting ${ship}`;
  ipcRenderer.send('boot-ship', ship);
}

ships.forEach(ship => {
  body += '<li>'
  body += `<a href="#" onclick=doship("${ship}")>`
  body += ship
  body += '</a>'
  body += '</li>'
});

body += '</ul>';

body += '<input id="bootname" placeholder="zod"></input>';

body += '<button type="button" onclick="mkship()">Boot new ship</button>'

document.body.innerHTML = body;

ipcRenderer.send('renderer-started', null);

ipcRenderer.on('ship-running', (event, ship, ports) => {
  console.log(ship, ports);
  redirectToShip(ports.ins);
});
