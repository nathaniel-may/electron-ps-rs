// THIS INITIALIZES THE ELECTRON APP
// The context bridge defines which function calls from the PureScript renderer
// are mapped to what IPC channels.

const { contextBridge, ipcRenderer } = require("electron/renderer");

contextBridge.exposeInMainWorld("electronAPI", {
  openFile: () => ipcRenderer.invoke("dialog:openFile"),
});
