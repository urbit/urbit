const { ipcRenderer } = require('electron');

const ships = ipcRenderer.sendSync('ship-list');

var body = '';

body += '<ul>';

const say = (html) => {
  let bod = document.body.innerHTML;
  bod += html
  document.body.innerHTML = bod;
}

const doship = (ship) => {
  say(`<br><br>Starting ${ship}...</br>`);
  ipcRenderer.send('start-ship', ship);
}

const mkship = () => {
  const ship = document.getElementById("bootname").value;
  say(`Booting ${ship}`);
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
  say(`<br><br>~${ship} is running!<br>`)
});
