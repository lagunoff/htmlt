import { absurd } from './lib';
import * as p from './protocol';
import { HaskellMessage, JavaScriptMessage, JavaScriptMessageTag, HaskellMessageTag, Bindings, List } from './protocol';

export function startDev(devSocketUri: string, startFlags: unknown = null) {
  const websocket = new WebSocket(devSocketUri);

  const sendToHaskell = async (jsMsg: JavaScriptMessage) => {
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
    haskellApp(haskMsg, null, sendToHaskell);
  };

  // Event handler for errors
  websocket.onerror = (event) => {
    console.error("WebSocket error:", event);
  };

  // Event handler for when the connection is closed
  websocket.onclose = (event) => {
    console.log("WebSocket connection closed:", event);
  };
}

export type HaskellAsyncCallback = (jsMsg: JavaScriptMessage, argScope: List<IArguments>) => Promise<void>;

export async function haskellApp (
  hmsg: HaskellMessage,
  argScope: List<IArguments>,
  send: HaskellAsyncCallback
): Promise<void> {
  switch (hmsg.tag) {
    case HaskellMessageTag.EvalExpr: {
      const result = p.evalExpr(send, globalContext, argScope, hmsg.expr);
      const jvalue = p.unknownToValue(result);
      const jmsg: JavaScriptMessage = { tag: JavaScriptMessageTag.Return, value: jvalue, threadId: hmsg.threadId };
      return send(jmsg, argScope);
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

const globalContext: List<Bindings> = [window as any, null]

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
