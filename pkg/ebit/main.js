/*
  Modules to control application life and create native browser window
*/
const {app, BrowserWindow} = require('electron')
const path = require('path')

console.log("Starting up");

/*
  Keep a global reference of the window object, if you don't, the window will
  be closed automatically when the JavaScript object is garbage collected.
*/
let mainWindow

/*
  - Create the browser window.
  - Load the index.html of the app.
  - Open the DevTools.

  - When window.closed is emitted:

    Dereference the window object, usually you would store windows in
    an array if your app supports multi windows, this is the time when
    you should delete the corresponding element.
*/
const createWindow = () => {
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  });

  mainWindow.loadFile('index.html');

  mainWindow.webContents.openDevTools();

  mainWindow.on('closed', () => {
    mainWindow = null;
  });
};

/*
  This method will be called when Electron has finished initialization
  and is ready to create browser windows.  Some APIs can only be used
  after this event occurs.
*/
app.on('ready', createWindow);

/*
  Quit when all windows are closed.

  On macOS it is common for applications and their menu bar to stay
  active until the user quits explicitly with Cmd + Q
*/
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
})

/*
  On macOS it's common to re-create a window in the app when the dock
  icon is clicked and there are no other windows open.
*/
app.on('activate', () => {
  if (mainWindow === null) createWindow();
})

/*
  In this file you can include the rest of your app's specific main
  process code. You can also put them in separate files and require
  them here.
*/
console.log("APP LOGIC HERE");
