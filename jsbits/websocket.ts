import { ClientMsgTag, EventId, PersistentState } from "./proto";
import * as proto from "./proto";

const outBuf = new ArrayBuffer(100 * 1024);
const mem = new DataView(outBuf);

export function runWebsocket(devSocketUri: string, startFlags: unknown = null) {
  const websocket = new WebSocket(devSocketUri);
  const persistent: PersistentState = {
    refs: new Map(),
    stack: null,
  };

  function triggerEvent(eventId: EventId, arg: unknown) {
    const encoderState = {mem, begin: 0, end: outBuf.byteLength};
    proto.encodeClientMessage(encoderState, [ClientMsgTag.EventMsg, eventId, arg]);
    websocket.send(new Uint8Array(outBuf).subarray(0, encoderState.end));
  };

  function resumeCont(contId: number, res: unknown) {
    const encoderState = {mem, begin: 0, end: outBuf.byteLength};
    proto.encodeClientMessage(encoderState, [ClientMsgTag.ResumeMsg, contId, res]);
    websocket.send(new Uint8Array(outBuf).subarray(0, encoderState.end));
  };

  websocket.onopen = (_event) => {
    const encoderState = {mem, begin: 0, end: outBuf.byteLength};
    proto.encodeClientMessage(encoderState, [ClientMsgTag.StartMsg, startFlags]);
    websocket.send(new Uint8Array(outBuf).subarray(0, encoderState.end));
  };

  // Event handler for receiving messages from the server
  websocket.onmessage = (event) => {
    convertBlobToUint8Array(event.data).then(buf => {
      proto.evalMem({
        triggerEvent,
        resumeCont,
        persistent,
        mem: new DataView(buf),
        isMutableMem: false,
        begin: 0,
        end: buf.byteLength,
      });
    });
  };

  // Event handler for errors
  websocket.onerror = (event) => {
    console.error("WebSocket error:", event);
  };

  // Event handler for when the connection is closed
  websocket.onclose = (_event) => {
    console.log("WebSocket connection closed, reloading the tab…");

    function backoffLoop(timeout: number) {
      // Assuming the server went down because it was re-compiled, wait
      // until it comes back and reload the tab
      const websocketTest = new WebSocket(devSocketUri);
      const nextTimeout = Math.min(30_000, timeout * 2);
      websocketTest.onopen = (_event) => window.location.reload();
      websocketTest.onclose = (_event) => { setTimeout(() => backoffLoop(nextTimeout), timeout); }
    }
    backoffLoop(100);
  };
}

export function convertBlobToUint8Array(blob: Blob): Promise<ArrayBuffer> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();

    reader.onload = () => {
      resolve(reader.result as ArrayBuffer);
    };

    reader.onerror = (error) => {
      reject(error);
    };

    reader.readAsArrayBuffer(blob);
  });
}
