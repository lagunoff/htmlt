import { absurd } from './lib';
import * as p from './protocol';
import { HaskellMessage, JavaScriptMessage, JavaScriptMessageTag, HaskellMessageTag } from './protocol';

export function startDev(devSocketUri: string, startFlags: unknown = null) {
  const websocket = new WebSocket(devSocketUri);

  const haskellCallback = async (jsMsg: JavaScriptMessage) => {
    websocket.send(p.javascriptMessage.encode(jsMsg));
    // const haskMsg = await awaitWebsocketMessage();
    // await haskellApp(haskMsg, argScope, sendToHaskell);
  };

  websocket.onopen = (_event) => {
    const startFlagsValue = p.unknownToValue(startFlags);
    const binaryData = p.javascriptMessage.encode({
      tag: JavaScriptMessageTag.Start,
      0: startFlagsValue
    });
    websocket.send(binaryData);
  };

  // Event handler for receiving messages from the server
  websocket.onmessage = async (event) => {
    const binaryDataReceived = await convertBlobToUint8Array(event.data);
    const haskMsg = p.haskellMessage.decode(binaryDataReceived);
    haskellApp(haskMsg, haskellCallback);
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

export type HaskellAsyncCallback = (jsMsg: JavaScriptMessage) => Promise<void>;

export async function haskellApp (
  hmsg: HaskellMessage,
  haskellCallback: HaskellAsyncCallback
): Promise<void> {
  switch (hmsg.tag) {
    case HaskellMessageTag.EvalExpr: {
      const result = p.evalExpr(hmsg.expr, { haskellCallback });
      const jvalue = p.unknownToValue(result);
      const jmsg: JavaScriptMessage = { tag: JavaScriptMessageTag.Return, value: jvalue, threadId: hmsg.threadId };
      return haskellCallback(jmsg);
    }
    case HaskellMessageTag.HotReload: {
      window.location.reload();
      return;
    }
    case HaskellMessageTag.Halt: {
      return;
    }
  }
  absurd(hmsg);
}

export function convertBlobToUint8Array(blob: Blob): Promise<Uint8Array> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();

    reader.onload = () => {
      const arrayBuffer = reader.result as ArrayBuffer;
      const uint8Array = new Uint8Array(arrayBuffer);
      resolve(uint8Array);
    };

    reader.onerror = (error) => {
      reject(error);
    };

    reader.readAsArrayBuffer(blob);
  });
}
