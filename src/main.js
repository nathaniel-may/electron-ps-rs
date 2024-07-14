// THIS IS ELECTRON "MAIN" PROCESS
// This file is the entry point to the "backend" of the electron app.
// Map ipc channel calls to function implementations in this file.

const { app, BrowserWindow, ipcMain, dialog } = require("electron/main");
const path = require("node:path");
const rust = require("../index.node");

function createWindow() {
  const mainWindow = new BrowserWindow({
    webPreferences: {
      preload: path.join(__dirname, "preload.js"),
    },
  });
  mainWindow.loadFile("index.html");

  // For debugging with browser tools
  // mainWindow.webContents.openDevTools();
}

app.whenReady().then(() => {
  ipcMain.handle("dialog:openFile", rust.pick_file);
  createWindow();
  app.on("activate", function () {
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on("window-all-closed", function () {
  if (process.platform !== "darwin") app.quit();
});
