module Clickable.Main.Dev where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Function
import Data.IORef
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Typeable
import Foreign.Store
import GHC.Generics
import GHC.IO.Exception
import Network.HTTP.Types as H
import Network.Wai as WAI
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import System.Environment
import System.IO

import "this" Clickable.Types
import "this" Clickable.Protocol
import "this" Clickable.Protocol.Value (Value(..))
import "this" Clickable.Internal (ClientMessage(..))
import "this" Clickable.Internal qualified as Internal


data DevConfig a = DevConfig
  { aquire_resource :: IO a
  -- ^ Usualy runs once just after ghci session is loaded,
  -- e.g. establish connection to database etc
  , release_resource :: a -> IO ()
  -- ^ Runs before the ghci session is unloaded
  , reload_app :: a -> IO ApplicationSpec
  -- ^ Given resource of type 'a', initialize instances of client and
  -- server applications. Runs each time ghci session reloads
  , html_template :: BSL.ByteString -> BSL.ByteString
  -- ^ Template for index.html, receives the current URL origin
  -- (protocol + host)
  , docroots :: [FilePath]
  -- ^ List of directories to use with wai-static middleware, could be
  -- empty, usually be used like docroots = ["./public"]
  } deriving Generic

data ApplicationSpec = ApplicationSpec
  { client_app :: ConnectionInfo -> StartFlags -> ClickM ()
  , server_app :: Application
  , connection_lost :: ConnectionInfo -> IO ()
  -- ^ Will be executed after a connection closes
  } deriving Generic

defaultConfig :: (ConnectionInfo -> StartFlags -> ClickM ()) -> DevConfig ()
defaultConfig clientApp = DevConfig
  { aquire_resource = pure ()
  , release_resource = const (pure ())
  , reload_app = \_ -> pure $ ApplicationSpec clientApp fallbackApp (const (pure ()))
  , html_template = htmlTemplate
  , docroots = []
  }

runSettings :: Typeable resource => Warp.Settings -> DevConfig resource -> IO ()
runSettings settings cfg = do
  -- Using a random constant as the key for Foreign.Store
  let storeId = 183
  hSetBuffering stderr LineBuffering
  lookupStore storeId >>= \case
    Nothing -> do
      inst <- newInstance cfg
      writeStore (Store storeId) inst
      let
        useCurrentApp req resp = do
          RunningApp{devserver_config, server_app} <- readIORef inst.app_state_ref
          withStaticApp devserver_config.docroots server_app req resp
      forkIfRepl $ tryPort settings $
        middleware inst useCurrentApp
    Just store -> do
      oldInst <- readStore store
      updateInstance cfg oldInst
      conns <- readIORef oldInst.conn_state_ref
      forM_ conns \conn ->
        sendDataMessage conn.connection . Binary $ Binary.encode HotReload
  where
    tryPort :: Warp.Settings -> Application -> IO ()
    tryPort settings application = do
      hPutStrLn stderr $ "Running a Dev Server at http://localhost:" <>
        show (Warp.getPort settings) <> "/"
      result <- try $ Warp.runSettings settings application
      case result of
        Right () -> return ()
        Left (e::IOException)
          | ioe_type e == ResourceBusy -> do
            hPutStrLn stderr $ "Already in use, trying next port…"
            tryPort (Warp.setPort (Warp.getPort settings + 1) settings) application
          | otherwise -> throwIO e
    withStaticApp :: [FilePath] -> Middleware
    withStaticApp [] next = next
    withStaticApp (docroot:docroots) next =
      staticApp (defaultFileServerSettings docroot)
        {ss404Handler = Just (withStaticApp docroots next)}
    forkIfRepl action = do
      inRepl <- (== "<interactive>") <$> getProgName
      if inRepl then void (forkIO action) else action

runDev :: (StartFlags -> ClickM ()) -> IO ()
runDev clientApp = runSettings
  (Warp.setPort 8080 Warp.defaultSettings)
  (defaultConfig (const clientApp))

middleware :: DevInstance -> Middleware
middleware opts next req resp =
  case pathInfo req of
    [] -> indexHtml req resp
    ["index.html"] -> indexHtml req resp
    ["dev.sock"] -> devserverApp req resp
    _ -> next req resp
  where
    devserverApp =
      websocketsOr defaultConnectionOptions (websocketApp opts)
      fallbackApp
    indexHtml req resp = do
      let devSocket = mkWebsocketUrl req
      RunningApp{devserver_config} <- readIORef opts.app_state_ref
      resp $ responseLBS status200
        [(hContentType, "text/html; charset=utf-8")] $
        devserver_config.html_template devSocket

mkWebsocketUrl :: WAI.Request -> BSL.ByteString
mkWebsocketUrl req =
  WAI.requestHeaders req
    & List.lookup "Host"
    & maybe "localhost" BSL.fromStrict
    & ((if WAI.isSecure req then "wss://" else "ws://") <>)
    & (<> "/dev.sock")

htmlTemplate :: BSL.ByteString -> BSL.ByteString
htmlTemplate devUrl =
  "<html>\n\
  \ <body>\n\
  \  <script>\n\
  \    " <> BSL.fromStrict indexBundleJs <> "\n\
  \    startDev(\"" <> devUrl <> "\");\n\
  \  </script>\n\
  \ </body>\n\
  \</html>\n"

fallbackApp :: Application
fallbackApp _ resp =
  resp $ responseLBS status404
    [(hContentType, "text/html; charset=utf-8")]
    "<html>\n\
    \ <body>\n\
    \   <h1>Not Found</h1>\n\
    \ </body>\n\
    \</html>\n"

-- WebSocket server needs to be redesigned. Specifically I don't like
-- the use of MVar to transfer the result of evaluation from the
-- browser to haskell thread
websocketApp :: DevInstance -> ServerApp
websocketApp opt p =
  let
    acceptConn = mdo
      conn <- acceptRequest p
      newConn conn
    dropConn connInfo = do
      modifyIORef' opt.conn_state_ref $ Map.delete connInfo.connection_id
      runningApp <- readIORef opt.app_state_ref
      runningApp.connection_lost connInfo
    receive c =
      try @ConnectionException (receiveData c)
    loop connInfo = do
      runningApp <- readIORef opt.app_state_ref
      raceResult <- race (receive connInfo.connection) (readChan connInfo.command_chan)
      case raceResult of
        Left (Right (incomingBytes::ByteString)) -> do
          let jsMessage = Binary.decode . BSL.fromStrict $ incomingBytes
          reader connInfo runningApp $ BrowserMessage jsMessage
          loop connInfo
        Left (Left (_::ConnectionException)) ->
          return ()
        Right jsAction -> do
          reader connInfo runningApp $ DevServerMessage jsAction
          loop connInfo

    reader :: ConnectionInfo -> RunningApp -> ClientMessage -> IO ()
    reader conn app = \case
      BrowserMessage (Start flags) -> void $ forkIO
        $ Internal.launchClickM conn.internal_env
        $ app.client_app conn flags
      BrowserMessage (Return val) -> void $ tryPutMVar conn.return_mvar val
      BrowserMessage (TriggerCallbackMsg arg sourceId) -> void $ forkIO
        $ Internal.launchClickM conn.internal_env
        $ modify (Internal.unsafeTrigger sourceId arg)
      BrowserMessage BeforeUnload ->
        return ()
      DevServerMessage a -> void $ forkIO
        $ Internal.launchClickM conn.internal_env a

    newConn connection = mdo
      command_chan <- newChan
      return_mvar <- newEmptyMVar
      internal_env <- newInternalEnv connection return_mvar
      connInfo <- atomicModifyIORef' opt.conn_state_ref \m ->
        let
          connection_id = Map.lookupMax m & maybe 0 (succ . fst)
          connInfo = ConnectionInfo
            { internal_env
            , connection
            , command_chan
            , connection_id
            , return_mvar }
        in
          (Map.insert connection_id connInfo m, connInfo)
      return connInfo
    newInternalEnv :: Connection -> MVar Value -> IO InternalEnv
    newInternalEnv conn resultMvar =
      Internal.newInternalEnv \haskMessage -> do
        sendDataMessage conn . Binary $ Binary.encode haskMessage
        Return <$> takeMVar resultMvar
  in
    bracket acceptConn dropConn \connInfo ->
      withPingThread connInfo.connection 30 (pure ()) $ loop connInfo

newInstance :: Typeable resource => DevConfig resource -> IO DevInstance
newInstance cfg = do
  resource <- cfg.aquire_resource
  appSpec <- cfg.reload_app resource
  app_state_ref <- newIORef RunningApp
    { resource
    , devserver_config = cfg
    , client_app = appSpec.client_app
    , server_app = appSpec.server_app
    , connection_lost = appSpec.connection_lost
    }
  conn_state_ref <- newIORef Map.empty
  return DevInstance {conn_state_ref, app_state_ref}

updateInstance
  :: Typeable resource
  => DevConfig resource
  -> DevInstance
  -> IO ()
updateInstance cfg devInst = do
  let
    tryOldResource :: forall a. Typeable a => DevConfig a ->
      RunningApp -> Either (IO ()) a
    tryOldResource _ RunningApp {resource, devserver_config}
      | Just Refl <- eqResource @a resource = Right resource
      | otherwise = Left (devserver_config.release_resource resource)
    eqResource :: forall a b. (Typeable a, Typeable b) => b -> Maybe (a :~: b)
    eqResource _ = eqT @a @b
  oldApp <- readIORef devInst.app_state_ref
  case tryOldResource cfg oldApp of
    Right oldResource -> do
      appSpec <- cfg.reload_app oldResource
      writeIORef devInst.app_state_ref RunningApp
        { resource = oldResource
        , devserver_config = cfg
        , client_app = appSpec.client_app
        , server_app = appSpec.server_app
        , connection_lost = appSpec.connection_lost
        }
    Left releaseOld -> do
      releaseOld
      newResource <- cfg.aquire_resource
      appSpec <- cfg.reload_app newResource
      writeIORef devInst.app_state_ref RunningApp
        { resource = newResource
        , devserver_config = cfg
        , client_app = appSpec.client_app
        , server_app = appSpec.server_app
        , connection_lost = appSpec.connection_lost
        }

data DevInstance = DevInstance
  { conn_state_ref :: IORef (Map ConnectionId ConnectionInfo)
  , app_state_ref :: IORef RunningApp
  } deriving Generic

data ConnectionInfo = ConnectionInfo
  { connection :: Connection
  , return_mvar :: MVar Value
  , internal_env :: InternalEnv
  , command_chan :: Chan (ClickM ())
  -- ^ Writing to the Chan sends a command to the browser to execute
  , connection_id :: ConnectionId
  } deriving Generic

data RunningApp = forall a. Typeable a => RunningApp
  { resource :: a
  , devserver_config :: DevConfig a
  , client_app :: ConnectionInfo -> StartFlags -> ClickM ()
  , server_app :: Application
  , connection_lost :: ConnectionInfo -> IO ()
  }

newtype ConnectionId = ConnectionId {unConnectionId :: Int}
  deriving newtype (Ord, Show, Eq, Num, Enum)

-- | Run @yarn run webpack --mode production@ and copy contents here
-- from @./dist-newstyle/index.bundle.js@
indexBundleJs :: ByteString
indexBundleJs = "(()=>{\"use strict\";var __webpack_modules__={924:(e,t,r)=>{r.d(t,{BN:()=>T,HA:()=>y,PV:()=>w,Tf:()=>P,YE:()=>h,YO:()=>M,Yj:()=>m,b9:()=>D,g1:()=>O,tB:()=>A});var n=r(366);class _{encode(e){const t=E(this,e),r=new Uint8Array(t);return b(this,r,0,e),r}decode(e){const[t,r]=g(this,e,0);return t}}class a extends _{}class s extends _{}class i extends _{}class o extends _{}class c extends _{}class l extends _{constructor(e){super(),this._element=e}}class p extends _{constructor(e){super(),this._description=e}}class u extends _{constructor(e){super(),this._alternatives=e}}class f extends _{constructor(e){super(),this._self=e}}class d extends _{constructor(e){super(),this._tuple=e}}function E(e,t){if(e instanceof a)return 1;if(e instanceof s)return 4;if(e instanceof i)return 8;if(e instanceof c){const e=t;return 8+(new TextEncoder).encode(e).length}if(e instanceof o)return 8+t.length;if(e instanceof l){const r=8;return t.reduce(((t,r)=>t+E(e._element,r)),r)}if(e instanceof p){const r=t;return Object.keys(e._description).reduce(((t,n)=>t+E(e._description[n],r[n])),0)}if(e instanceof u){const r=t;return x(Object.keys(e._alternatives).length)+E(e._alternatives[r.tag],r)}if(e instanceof f)return E(e._self,t);if(e instanceof d){const r=t;return e._tuple.reduce(((e,t,n)=>e+E(t,r[n])),0)}return(0,n.G)(e)}function g(e,t,r){if(e instanceof a)return[t[r],r+1];if(e instanceof s)return[new DataView(t.buffer).getInt32(r,!0),r+4];if(e instanceof i)return[new DataView(t.buffer).getFloat64(r,!0),r+8];if(e instanceof c){const e=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),n=t.subarray(r+8,r+8+e);return[new TextDecoder(\"utf8\").decode(n),r+8+e]}if(e instanceof o){const e=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56);return[t.subarray(r+8,r+8+e),r+8+e]}if(e instanceof l){const n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),_=[];let a=r+8;for(let r=0;r<n;r++){const[r,n]=g(e._element,t,a);_.push(r),a=n}return[_,a]}if(e instanceof p){let n=r;return[Object.fromEntries(Object.entries(e._description).map((([e,r])=>{const[_,a]=g(r,t,n);return n=a,[e,_]}))),n]}if(e instanceof u){const n=x(Object.keys(e._alternatives).length),[_,a]=function(e,t,r){if(1!=e)throw new Error(\"Unimplemented\");return[t[r],r+1]}(n,t,r),[s,i]=g(e._alternatives[_],t,a);return s.tag=_,[s,i]}if(e instanceof f)return g(e._self,t,r);if(e instanceof d){let n=r;return[e._tuple.map((e=>{const[r,_]=g(e,t,n);return n=_,r})),n]}return(0,n.G)(e)}function b(e,t,r,_){if(e instanceof a)return t[r]=_,r+1;if(e instanceof s)return new DataView(t.buffer).setInt32(r,_,!0),r+4;if(e instanceof i)return new DataView(t.buffer).setFloat64(r,_,!0),r+8;if(e instanceof c){const e=_,n=(new TextEncoder).encode(e),a=n.length;return t[r+7]=255&a,t[r+6]=a>>8&255,t[r+5]=a>>16&255,t[r+4]=a>>24&255,t.set(n,r+8),r+8+a}if(e instanceof o){const e=_,n=e.length;return t[r+7]=255&n,t[r+6]=n>>8&255,t[r+5]=n>>16&255,t[r+4]=n>>24&255,t.set(e,r+8),r+8+n}if(e instanceof l){const n=_,a=n.length;t[r+7]=255&a,t[r+6]=a>>8&255,t[r+5]=a>>16&255,t[r+4]=a>>24&255;let s=r+8;for(let r=0;r<a;r++)s=b(e._element,t,s,n[r]);return s}if(e instanceof p){const n=_;let a=r;for(const r in e._description)Object.prototype.hasOwnProperty.call(e._description,r)&&(a=b(e._description[r],t,a,n[r]));return a}if(e instanceof u){const n=_.tag,a=x(Object.keys(e._alternatives).length);return t[r]=n,b(e._alternatives[n],t,r+a,_)}if(e instanceof f)return b(e._self,t,r,_);if(e instanceof d){const n=_;let a=r;return e._tuple.forEach(((e,r)=>{a=b(e,t,a,n[r])})),a}return(0,n.G)(e)}function x(e){return Math.ceil(Math.log2(e)/8)}const h=new a,y=new s,T=new i,m=new c,D=new o;function M(e){return new l(e)}function O(e){return new p(e)}function P(e){return new u(e)}function w(...e){return new d(e)}function A(e){const t=new f(void 0),r=e(t);return t._self=r,r}var I;!function(e){e[e.Nothing=0]=\"Nothing\",e[e.Just=1]=\"Just\"}(I||(I={}))},366:(e,t,r)=>{function n(e){throw new Error(\"absurd: unreachable code\")}r.d(t,{G:()=>n,Y:()=>_});class _{constructor(){this.counter=0,this.map=new Map}push(e){const t=this.counter++;return this.map.set(t,e),t}set(e,t){this.map.set(e,t)}get(e){return this.map.get(e)}delete(e){return this.map.delete(e)}has(e){return this.map.has(e)}clear(){this.map.clear()}keys(){return this.map.keys()}values(){return this.map.values()}entries(){return this.map.entries()}forEach(e,t){this.map.forEach(e,t)}}},933:(__unused_webpack_module,__webpack_exports__,__webpack_require__)=>{__webpack_require__.d(__webpack_exports__,{I4:()=>HaskellMessageTag,bR:()=>javascriptMessage,bp:()=>unknownToValue,dB:()=>JavaScriptMessageTag,sy:()=>haskellMessage,wL:()=>evalExpr});var _binary__WEBPACK_IMPORTED_MODULE_0__=__webpack_require__(924),_lib__WEBPACK_IMPORTED_MODULE_1__=__webpack_require__(366),ValueTag;function Cons(e,t){return[e,t]}function car(e){return e[0]}function cdr(e){return e[1]}function evalExpr(hscb,idenScope,argScope,exp){var _a;switch(exp.tag){case ExprTag.Null:return null;case ExprTag.Boolean:return 0!=exp[0];case ExprTag.I32:case ExprTag.F64:case ExprTag.String:return exp[0];case ExprTag.Array:return exp[0].map(evalExpr.bind(void 0,hscb,idenScope,argScope));case ExprTag.Object:return Object.fromEntries(exp[0].map((([e,t])=>[e,evalExpr(hscb,idenScope,argScope,t)])));case ExprTag.Uint8Array:return exp[0];case ExprTag.Dot:{const e=evalExpr(hscb,idenScope,argScope,exp[0]);return e[exp[1]]}case ExprTag.SetProp:{const e=evalExpr(hscb,idenScope,argScope,exp[2]),t=evalExpr(hscb,idenScope,argScope,exp[0]);return t[exp[1]]=e,e}case ExprTag.Ix:{const e=evalExpr(hscb,idenScope,argScope,exp.exp);return e[exp.ix]}case ExprTag.Plus:{const e=evalExpr(hscb,idenScope,argScope,exp[0]),t=evalExpr(hscb,idenScope,argScope,exp[1]);return e+t}case ExprTag.Subtract:{const e=evalExpr(hscb,idenScope,argScope,exp[0]),t=evalExpr(hscb,idenScope,argScope,exp[1]);return e-t}case ExprTag.Multiply:{const e=evalExpr(hscb,idenScope,argScope,exp[0]),t=evalExpr(hscb,idenScope,argScope,exp[1]);return e*t}case ExprTag.Divide:{const e=evalExpr(hscb,idenScope,argScope,exp[0]),t=evalExpr(hscb,idenScope,argScope,exp[1]);return e/t}case ExprTag.Id:{const e=exp[0];for(let t=idenScope;t;t=cdr(t)){const r=car(t);if(e in r)return r[e]}throw new Error(\"Variable not in scope: \"+exp[0])}case ExprTag.Lam:return function(){return evalExpr(hscb,idenScope,Cons(arguments,argScope),exp.body)};case ExprTag.Arg:{let e=argScope,t=0;for(;e;){if(t==exp.scopeIx){const t=car(e);return t[exp.argIx]}e=cdr(e),t++}throw new Error(\"Argument scope out of a range: \"+exp.scopeIx)}case ExprTag.Apply:{const e=evalExpr(hscb,idenScope,argScope,exp[0]);return e.apply(void 0,exp[1].map(evalExpr.bind(void 0,hscb,idenScope,argScope)))}case ExprTag.Call:{const e=evalExpr(hscb,idenScope,argScope,exp[0]),t=e[exp[1]];return t.apply(e,exp[2].map(evalExpr.bind(void 0,hscb,idenScope,argScope)))}case ExprTag.AssignVar:{const e=evalExpr(hscb,idenScope,argScope,exp.rhs);if(varStorage.has(exp.scopeId)){const t=varStorage.get(exp.scopeId);t.set(exp.varId,e)}else{const t=new Map;t.set(exp.varId,e),varStorage.set(exp.scopeId,t)}return e}case ExprTag.FreeVar:{const e=varStorage.get(exp.scopeId);if(!e)return;return e.delete(exp.varId),void(0==e.size&&varStorage.delete(exp.scopeId))}case ExprTag.Var:return null===(_a=varStorage.get(exp.scopeId))||void 0===_a?void 0:_a.get(exp.varId);case ExprTag.FreeScope:{varStorage.delete(exp.scopeId);const e=finalizers.get(exp.scopeId);return e&&e.forEach((e=>e())),null}case ExprTag.InsertNode:{const e=evalExpr(hscb,idenScope,argScope,exp.parent),t=evalExpr(hscb,idenScope,argScope,exp.child);return domHelpers.insertIntoBuilder(e,t),null}case ExprTag.CreateElement:return document.createElement(exp.tagName);case ExprTag.CreateElementNS:return document.createElementNS(exp.ns,exp.tagName);case ExprTag.CreateText:return document.createTextNode(exp.content);case ExprTag.ElementProp:{const e=evalExpr(hscb,idenScope,argScope,exp.node),t=evalExpr(hscb,idenScope,argScope,exp.propValue);return domHelpers.assignProperty(e,exp.propName,t),null}case ExprTag.ElementAttr:{const e=evalExpr(hscb,idenScope,argScope,exp.node);return domHelpers.assignAttribute(e,exp.attrName,exp.attrValue),null}case ExprTag.InsertClassList:{const e=evalExpr(hscb,idenScope,argScope,exp.node),t=domHelpers.domBuilderElement(e);return exp.classList.forEach((e=>t.classList.add(e))),null}case ExprTag.RemoveClassList:{const e=evalExpr(hscb,idenScope,argScope,exp.node),t=domHelpers.domBuilderElement(e);return exp.classList.forEach((e=>t.classList.remove(e))),null}case ExprTag.UpdateTextNode:{const e=evalExpr(hscb,idenScope,argScope,exp.node);return e.textContent=exp.content,null}case ExprTag.InsertBoundary:{const e=evalExpr(hscb,idenScope,argScope,exp.parent);return domHelpers.insertBoundary(e)}case ExprTag.ClearBoundary:{const e=evalExpr(hscb,idenScope,argScope,exp.boundary);return domHelpers.clearBoundary(e,Boolean(exp.detach))}case ExprTag.AddEventListener:{const e=evalExpr(hscb,idenScope,argScope,exp.target),t=evalExpr(hscb,idenScope,argScope,exp.eventName),r=evalExpr(hscb,idenScope,argScope,exp.listener);domHelpers.addEventListener(e,t,r);const n=finalizers.get(exp.reactiveScope),_=n||new _lib__WEBPACK_IMPORTED_MODULE_1__.Y;return n||finalizers.set(exp.reactiveScope,_),_.push((()=>domHelpers.removeEventListener(e,t,r)))}case ExprTag.ConnectResource:{const e=evalExpr(hscb,idenScope,argScope,exp.aquire),t=finalizers.get(exp.reactiveScope),r=t||new _lib__WEBPACK_IMPORTED_MODULE_1__.Y;return t||finalizers.set(exp.reactiveScope,r),r.push(e)}case ExprTag.SetTimeout:{const e=evalExpr(hscb,idenScope,argScope,exp.callback),t=finalizers.get(exp.reactiveScope),r=t||new _lib__WEBPACK_IMPORTED_MODULE_1__.Y;t||finalizers.set(exp.reactiveScope,r);let n=null;const _=r.push((()=>n&&clearTimeout(n)));return n=setTimeout((()=>{r.delete(_),n=null,e()}),exp.timeout),_}case ExprTag.ApplyFinalizer:{const e=finalizers.get(exp.reactiveScope),t=evalExpr(hscb,idenScope,argScope,exp.finalizerId);if(!e)return!1;const r=e.get(t);return!!r&&(e.delete(t),r(),!0)}case ExprTag.RevSeq:return exp.exprs.reduceRight(((e,t)=>evalExpr(hscb,idenScope,argScope,t)),null);case ExprTag.Eval:return eval(exp.rawJavaScript);case ExprTag.TriggerCallback:{const e=evalExpr(hscb,idenScope,argScope,exp.arg),t={tag:JavaScriptMessageTag.TriggerCallback,arg:unknownToValue(e),callbackId:exp.callbackId};return hscb(t,argScope)}case ExprTag.UncaughtException:throw new Error(exp.message)}(0,_lib__WEBPACK_IMPORTED_MODULE_1__.G)(exp)}function unknownToValue(e){if(\"boolean\"==typeof e)return{tag:ValueTag.Boolean,0:e?1:0};if(\"number\"==typeof e)return Number.isInteger(e)?{tag:ValueTag.I32,0:e}:{tag:ValueTag.F64,0:e};if(\"string\"==typeof e)return{tag:ValueTag.String,0:e};if(Array.isArray(e))return{tag:ValueTag.Array,0:e.map(unknownToValue)};if(e instanceof Uint8Array)return{tag:ValueTag.Uint8Array,0:e};if(null==e)return{tag:ValueTag.Null};const t=Object.entries(e).map((([e,t])=>[e,unknownToValue(t)]));return{tag:ValueTag.Object,0:t}}!function(e){e[e.Null=0]=\"Null\",e[e.Boolean=1]=\"Boolean\",e[e.I32=2]=\"I32\",e[e.F64=3]=\"F64\",e[e.String=4]=\"String\",e[e.Array=5]=\"Array\",e[e.Object=6]=\"Object\",e[e.Uint8Array=7]=\"Uint8Array\"}(ValueTag||(ValueTag={}));const jvalue=_binary__WEBPACK_IMPORTED_MODULE_0__.tB((e=>_binary__WEBPACK_IMPORTED_MODULE_0__.Tf({[ValueTag.Null]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({}),[ValueTag.Boolean]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ValueTag.I32]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ValueTag.F64]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.BN}),[ValueTag.String]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ValueTag.Array]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ValueTag.Object]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(_binary__WEBPACK_IMPORTED_MODULE_0__.PV(_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,e))}),[ValueTag.Uint8Array]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.b9})})));var ExprTag;!function(e){e[e.Null=0]=\"Null\",e[e.Boolean=1]=\"Boolean\",e[e.I32=2]=\"I32\",e[e.F64=3]=\"F64\",e[e.String=4]=\"String\",e[e.Array=5]=\"Array\",e[e.Object=6]=\"Object\",e[e.Uint8Array=7]=\"Uint8Array\",e[e.Dot=8]=\"Dot\",e[e.SetProp=9]=\"SetProp\",e[e.Ix=10]=\"Ix\",e[e.Plus=11]=\"Plus\",e[e.Subtract=12]=\"Subtract\",e[e.Multiply=13]=\"Multiply\",e[e.Divide=14]=\"Divide\",e[e.Id=15]=\"Id\",e[e.Lam=16]=\"Lam\",e[e.Arg=17]=\"Arg\",e[e.Apply=18]=\"Apply\",e[e.Call=19]=\"Call\",e[e.AssignVar=20]=\"AssignVar\",e[e.FreeVar=21]=\"FreeVar\",e[e.Var=22]=\"Var\",e[e.FreeScope=23]=\"FreeScope\",e[e.InsertNode=24]=\"InsertNode\",e[e.CreateElement=25]=\"CreateElement\",e[e.CreateElementNS=26]=\"CreateElementNS\",e[e.CreateText=27]=\"CreateText\",e[e.ElementProp=28]=\"ElementProp\",e[e.ElementAttr=29]=\"ElementAttr\",e[e.InsertClassList=30]=\"InsertClassList\",e[e.RemoveClassList=31]=\"RemoveClassList\",e[e.UpdateTextNode=32]=\"UpdateTextNode\",e[e.InsertBoundary=33]=\"InsertBoundary\",e[e.ClearBoundary=34]=\"ClearBoundary\",e[e.AddEventListener=35]=\"AddEventListener\",e[e.ConnectResource=36]=\"ConnectResource\",e[e.SetTimeout=37]=\"SetTimeout\",e[e.ApplyFinalizer=38]=\"ApplyFinalizer\",e[e.RevSeq=39]=\"RevSeq\",e[e.Eval=40]=\"Eval\",e[e.TriggerCallback=41]=\"TriggerCallback\",e[e.UncaughtException=42]=\"UncaughtException\"}(ExprTag||(ExprTag={}));const expr=_binary__WEBPACK_IMPORTED_MODULE_0__.tB((e=>_binary__WEBPACK_IMPORTED_MODULE_0__.Tf({[ExprTag.Null]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({}),[ExprTag.Boolean]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ExprTag.I32]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.F64]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.BN}),[ExprTag.String]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.Array]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ExprTag.Object]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(_binary__WEBPACK_IMPORTED_MODULE_0__.PV(_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,e))}),[ExprTag.Dot]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.SetProp]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,2:e}),[ExprTag.Ix]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({exp:e,ix:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.Plus]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:e}),[ExprTag.Subtract]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:e}),[ExprTag.Multiply]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:e}),[ExprTag.Divide]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:e}),[ExprTag.Id]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.Lam]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({body:e}),[ExprTag.Arg]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeIx:_binary__WEBPACK_IMPORTED_MODULE_0__.YE,argIx:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ExprTag.Apply]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ExprTag.Call]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,2:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ExprTag.AssignVar]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,varId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,rhs:e}),[ExprTag.FreeVar]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,varId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.Var]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,varId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.FreeScope]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.InsertNode]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({parent:e,child:e}),[ExprTag.CreateElement]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({tagName:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.CreateElementNS]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({ns:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,tagName:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.CreateText]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({content:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.ElementProp]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({node:e,propName:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,propValue:e}),[ExprTag.ElementAttr]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({node:e,attrName:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,attrValue:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.InsertClassList]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({node:e,classList:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(_binary__WEBPACK_IMPORTED_MODULE_0__.Yj)}),[ExprTag.RemoveClassList]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({node:e,classList:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(_binary__WEBPACK_IMPORTED_MODULE_0__.Yj)}),[ExprTag.UpdateTextNode]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({node:e,content:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.InsertBoundary]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({parent:e}),[ExprTag.ClearBoundary]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({boundary:e,detach:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ExprTag.AddEventListener]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({reactiveScope:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,target:e,eventName:e,listener:e}),[ExprTag.ConnectResource]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({reactiveScope:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,aquire:e}),[ExprTag.SetTimeout]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({reactiveScope:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,callback:e,timeout:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.ApplyFinalizer]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({reactiveScope:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,finalizerId:e}),[ExprTag.RevSeq]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({exprs:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ExprTag.Eval]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({rawJavaScript:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.TriggerCallback]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({callbackId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,arg:e}),[ExprTag.UncaughtException]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({message:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj})})));var HaskellMessageTag;!function(e){e[e.EvalExpr=0]=\"EvalExpr\",e[e.HotReload=1]=\"HotReload\",e[e.Halt=2]=\"Halt\"}(HaskellMessageTag||(HaskellMessageTag={}));const haskellMessage=_binary__WEBPACK_IMPORTED_MODULE_0__.Tf({[HaskellMessageTag.EvalExpr]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({expr}),[HaskellMessageTag.HotReload]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({}),[HaskellMessageTag.Halt]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({})});var JavaScriptMessageTag;!function(e){e[e.Start=0]=\"Start\",e[e.Return=1]=\"Return\",e[e.TriggerCallback=2]=\"TriggerCallback\",e[e.BeforeUnload=3]=\"BeforeUnload\"}(JavaScriptMessageTag||(JavaScriptMessageTag={}));const javascriptMessage=_binary__WEBPACK_IMPORTED_MODULE_0__.Tf({[JavaScriptMessageTag.Start]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:jvalue}),[JavaScriptMessageTag.Return]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:jvalue}),[JavaScriptMessageTag.TriggerCallback]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({arg:jvalue,callbackId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[JavaScriptMessageTag.BeforeUnload]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({})}),varStorage=new Map,finalizers=new Map;var domHelpers;!function(e){function t(e,t){e instanceof Comment?e.parentElement.insertBefore(t,e):e.appendChild(t)}function r(e){return e instanceof Comment?e.parentElement:e}function n(e){return e instanceof Comment&&\"ContentBoundary {{\"==e.textContent}e.insertIntoBuilder=t,e.assignProperty=function(e,t,r){e instanceof Comment?e.parentElement[t]=r:e[t]=r},e.assignAttribute=function(e,t,n){r(e).setAttribute(t,n)},e.addEventListener=function(e,t,n){r(e).addEventListener(t,n)},e.removeEventListener=function(e,t,n){r(e).removeEventListener(t,n)},e.insertBoundary=function(e){const r=document.createComment(\"ContentBoundary {{\"),n=document.createComment(\"}}\");return t(e,r),t(e,n),n},e.clearBoundary=function(e,t){const r=e;let _=0;for(;r.previousSibling&&(0!=_||!n(r.previousSibling));)(a=r.previousSibling)instanceof Comment&&\"}}\"==a.textContent?_++:n(r.previousSibling)&&_--,r.previousSibling.parentNode.removeChild(r.previousSibling);var a;t&&(r.previousSibling.parentNode.removeChild(r.previousSibling),r.parentNode.removeChild(r))},e.domBuilderElement=r}(domHelpers||(domHelpers={}))}},__webpack_module_cache__={};function __webpack_require__(e){var t=__webpack_module_cache__[e];if(void 0!==t)return t.exports;var r=__webpack_module_cache__[e]={exports:{}};return __webpack_modules__[e](r,r.exports,__webpack_require__),r.exports}__webpack_require__.d=(e,t)=>{for(var r in t)__webpack_require__.o(t,r)&&!__webpack_require__.o(e,r)&&Object.defineProperty(e,r,{enumerable:!0,get:t[r]})},__webpack_require__.o=(e,t)=>Object.prototype.hasOwnProperty.call(e,t);var __webpack_exports__={};(()=>{function e(e,t,r,n){return new(r||(r=Promise))((function(_,a){function s(e){try{o(n.next(e))}catch(e){a(e)}}function i(e){try{o(n.throw(e))}catch(e){a(e)}}function o(e){var t;e.done?_(e.value):(t=e.value,t instanceof r?t:new r((function(e){e(t)}))).then(s,i)}o((n=n.apply(e,t||[])).next())}))}const t=58;class r{static read_bytes(e,t){const n=new r;return n.buf=e.getUint32(t,!0),n.buf_len=e.getUint32(t+4,!0),n}static read_bytes_array(e,t,n){const _=[];for(let a=0;a<n;a++)_.push(r.read_bytes(e,t+8*a));return _}}class n{static read_bytes(e,t){const r=new n;return r.buf=e.getUint32(t,!0),r.buf_len=e.getUint32(t+4,!0),r}static read_bytes_array(e,t,r){const _=[];for(let a=0;a<r;a++)_.push(n.read_bytes(e,t+8*a));return _}}class _{write_bytes(e,t){e.setUint8(t,this.fs_filetype),e.setUint16(t+2,this.fs_flags,!0),e.setBigUint64(t+8,this.fs_rights_base,!0),e.setBigUint64(t+16,this.fs_rights_inherited,!0)}constructor(e,t){this.fs_rights_base=0n,this.fs_rights_inherited=0n,this.fs_filetype=e,this.fs_flags=t}}class a{write_bytes(e,t){e.setBigUint64(t,this.dev,!0),e.setBigUint64(t+8,this.ino,!0),e.setUint8(t+16,this.filetype),e.setBigUint64(t+24,this.nlink,!0),e.setBigUint64(t+32,this.size,!0),e.setBigUint64(t+38,this.atim,!0),e.setBigUint64(t+46,this.mtim,!0),e.setBigUint64(t+52,this.ctim,!0)}constructor(e,t){this.dev=0n,this.ino=0n,this.nlink=0n,this.atim=0n,this.mtim=0n,this.ctim=0n,this.filetype=e,this.size=t}}const s=new class{enable(e){this.log=function(e,t){return e?console.log.bind(console,\"%c%s\",\"color: #265BA0\",t):()=>{}}(void 0===e||e,this.prefix)}get enabled(){return this.isEnabled}constructor(e){this.isEnabled=e,this.prefix=\"wasi:\",this.enable(e)}}(!1);class i extends Error{constructor(e){super(\"exit with exit code \"+e),this.code=e}}let o,c=class{start(e){this.inst=e;try{return e.exports._start(),0}catch(e){if(e instanceof i)return e.code;throw e}}initialize(e){this.inst=e,e.exports._initialize()}constructor(e,t,_,a={}){this.args=[],this.env=[],this.fds=[],s.enable(a.debug),this.args=e,this.env=t,this.fds=_;const o=this;this.wasiImport={args_sizes_get(e,t){const r=new DataView(o.inst.exports.memory.buffer);r.setUint32(e,o.args.length,!0);let n=0;for(const e of o.args)n+=e.length+1;return r.setUint32(t,n,!0),s.log(r.getUint32(e,!0),r.getUint32(t,!0)),0},args_get(e,t){const r=new DataView(o.inst.exports.memory.buffer),n=new Uint8Array(o.inst.exports.memory.buffer),_=t;for(let _=0;_<o.args.length;_++){r.setUint32(e,t,!0),e+=4;const a=(new TextEncoder).encode(o.args[_]);n.set(a,t),r.setUint8(t+a.length,0),t+=a.length+1}return s.enabled&&s.log(new TextDecoder(\"utf-8\").decode(n.slice(_,t))),0},environ_sizes_get(e,t){const r=new DataView(o.inst.exports.memory.buffer);r.setUint32(e,o.env.length,!0);let n=0;for(const e of o.env)n+=e.length+1;return r.setUint32(t,n,!0),s.log(r.getUint32(e,!0),r.getUint32(t,!0)),0},environ_get(e,t){const r=new DataView(o.inst.exports.memory.buffer),n=new Uint8Array(o.inst.exports.memory.buffer),_=t;for(let _=0;_<o.env.length;_++){r.setUint32(e,t,!0),e+=4;const a=(new TextEncoder).encode(o.env[_]);n.set(a,t),r.setUint8(t+a.length,0),t+=a.length+1}return s.enabled&&s.log(new TextDecoder(\"utf-8\").decode(n.slice(_,t))),0},clock_res_get(e,t){let r;switch(e){case 1:r=5000n;break;case 0:r=1000000n;break;default:return 52}return new DataView(o.inst.exports.memory.buffer).setBigUint64(t,r,!0),0},clock_time_get(e,t,r){const n=new DataView(o.inst.exports.memory.buffer);if(0===e)n.setBigUint64(r,1000000n*BigInt((new Date).getTime()),!0);else if(1==e){let e;try{e=BigInt(Math.round(1e6*performance.now()))}catch(t){e=0n}n.setBigUint64(r,e,!0)}else n.setBigUint64(r,0n,!0);return 0},fd_advise:(e,t,r,n)=>null!=o.fds[e]?o.fds[e].fd_advise(t,r,n):8,fd_allocate:(e,t,r)=>null!=o.fds[e]?o.fds[e].fd_allocate(t,r):8,fd_close(e){if(null!=o.fds[e]){const t=o.fds[e].fd_close();return o.fds[e]=void 0,t}return 8},fd_datasync:e=>null!=o.fds[e]?o.fds[e].fd_datasync():8,fd_fdstat_get(e,t){if(null!=o.fds[e]){const{ret:r,fdstat:n}=o.fds[e].fd_fdstat_get();return null!=n&&n.write_bytes(new DataView(o.inst.exports.memory.buffer),t),r}return 8},fd_fdstat_set_flags:(e,t)=>null!=o.fds[e]?o.fds[e].fd_fdstat_set_flags(t):8,fd_fdstat_set_rights:(e,t,r)=>null!=o.fds[e]?o.fds[e].fd_fdstat_set_rights(t,r):8,fd_filestat_get(e,t){if(null!=o.fds[e]){const{ret:r,filestat:n}=o.fds[e].fd_filestat_get();return null!=n&&n.write_bytes(new DataView(o.inst.exports.memory.buffer),t),r}return 8},fd_filestat_set_size:(e,t)=>null!=o.fds[e]?o.fds[e].fd_filestat_set_size(t):8,fd_filestat_set_times:(e,t,r,n)=>null!=o.fds[e]?o.fds[e].fd_filestat_set_times(t,r,n):8,fd_pread(e,t,n,_,a){const s=new DataView(o.inst.exports.memory.buffer),i=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const c=r.read_bytes_array(s,t,n),{ret:l,nread:p}=o.fds[e].fd_pread(i,c,_);return s.setUint32(a,p,!0),l}return 8},fd_prestat_get(e,t){const r=new DataView(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const{ret:n,prestat:_}=o.fds[e].fd_prestat_get();return null!=_&&_.write_bytes(r,t),n}return 8},fd_prestat_dir_name(e,t,r){if(null!=o.fds[e]){const{ret:r,prestat_dir_name:n}=o.fds[e].fd_prestat_dir_name();return null!=n&&new Uint8Array(o.inst.exports.memory.buffer).set(n,t),r}return 8},fd_pwrite(e,t,r,_,a){const s=new DataView(o.inst.exports.memory.buffer),i=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const c=n.read_bytes_array(s,t,r),{ret:l,nwritten:p}=o.fds[e].fd_pwrite(i,c,_);return s.setUint32(a,p,!0),l}return 8},fd_read(e,t,n,_){const a=new DataView(o.inst.exports.memory.buffer),s=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const i=r.read_bytes_array(a,t,n),{ret:c,nread:l}=o.fds[e].fd_read(s,i);return a.setUint32(_,l,!0),c}return 8},fd_readdir(e,t,r,n,_){const a=new DataView(o.inst.exports.memory.buffer),s=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){let i=0;for(;;){const{ret:c,dirent:l}=o.fds[e].fd_readdir_single(n);if(0!=c)return a.setUint32(_,i,!0),c;if(null==l)break;if(r-i<l.head_length()){i=r;break}const p=new ArrayBuffer(l.head_length());if(l.write_head_bytes(new DataView(p),0),s.set(new Uint8Array(p).slice(0,Math.min(p.byteLength,r-i)),t),t+=l.head_length(),i+=l.head_length(),r-i<l.name_length()){i=r;break}l.write_name_bytes(s,t,r-i),t+=l.name_length(),i+=l.name_length(),n=l.d_next}return a.setUint32(_,i,!0),0}return 8},fd_renumber(e,t){if(null!=o.fds[e]&&null!=o.fds[t]){const r=o.fds[t].fd_close();return 0!=r?r:(o.fds[t]=o.fds[e],o.fds[e]=void 0,0)}return 8},fd_seek(e,t,r,n){const _=new DataView(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const{ret:a,offset:s}=o.fds[e].fd_seek(t,r);return _.setBigInt64(n,s,!0),a}return 8},fd_sync:e=>null!=o.fds[e]?o.fds[e].fd_sync():8,fd_tell(e,t){const r=new DataView(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const{ret:n,offset:_}=o.fds[e].fd_tell();return r.setBigUint64(t,_,!0),n}return 8},fd_write(e,t,r,_){const a=new DataView(o.inst.exports.memory.buffer),s=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const i=n.read_bytes_array(a,t,r),{ret:c,nwritten:l}=o.fds[e].fd_write(s,i);return a.setUint32(_,l,!0),c}return 8},path_create_directory(e,t,r){const n=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const _=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return o.fds[e].path_create_directory(_)}},path_filestat_get(e,t,r,n,_){const a=new DataView(o.inst.exports.memory.buffer),s=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const i=new TextDecoder(\"utf-8\").decode(s.slice(r,r+n)),{ret:c,filestat:l}=o.fds[e].path_filestat_get(t,i);return null!=l&&l.write_bytes(a,_),c}return 8},path_filestat_set_times(e,t,r,n,_,a,s){const i=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const c=new TextDecoder(\"utf-8\").decode(i.slice(r,r+n));return o.fds[e].path_filestat_set_times(t,c,_,a,s)}return 8},path_link(e,t,r,n,_,a,s){const i=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]&&null!=o.fds[_]){const c=new TextDecoder(\"utf-8\").decode(i.slice(r,r+n)),l=new TextDecoder(\"utf-8\").decode(i.slice(a,a+s));return o.fds[_].path_link(e,t,c,l)}return 8},path_open(e,t,r,n,_,a,i,c,l){const p=new DataView(o.inst.exports.memory.buffer),u=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const f=new TextDecoder(\"utf-8\").decode(u.slice(r,r+n));s.log(f);const{ret:d,fd_obj:E}=o.fds[e].path_open(t,f,_,a,i,c);if(0!=d)return d;o.fds.push(E);const g=o.fds.length-1;return p.setUint32(l,g,!0),0}return 8},path_readlink(e,t,r,n,_,a){const i=new DataView(o.inst.exports.memory.buffer),c=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const l=new TextDecoder(\"utf-8\").decode(c.slice(t,t+r));s.log(l);const{ret:p,data:u}=o.fds[e].path_readlink(l);if(null!=u){const e=(new TextEncoder).encode(u);if(e.length>_)return i.setUint32(a,0,!0),8;c.set(e,n),i.setUint32(a,e.length,!0)}return p}return 8},path_remove_directory(e,t,r){const n=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const _=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return o.fds[e].path_remove_directory(_)}return 8},path_rename(e,t,r,n,_,a){throw\"FIXME what is the best abstraction for this?\"},path_symlink(e,t,r,n,_){const a=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[r]){const s=new TextDecoder(\"utf-8\").decode(a.slice(e,e+t)),i=new TextDecoder(\"utf-8\").decode(a.slice(n,n+_));return o.fds[r].path_symlink(s,i)}return 8},path_unlink_file(e,t,r){const n=new Uint8Array(o.inst.exports.memory.buffer);if(null!=o.fds[e]){const _=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return o.fds[e].path_unlink_file(_)}return 8},poll_oneoff(e,t,r){throw\"async io not supported\"},proc_exit(e){throw new i(e)},proc_raise(e){throw\"raised signal \"+e},sched_yield(){},random_get(e,t){const r=new Uint8Array(o.inst.exports.memory.buffer);for(let n=0;n<t;n++)r[e+n]=256*Math.random()|0},sock_recv(e,t,r){throw\"sockets not supported\"},sock_send(e,t,r){throw\"sockets not supported\"},sock_shutdown(e,t){throw\"sockets not supported\"},sock_accept(e,t){throw\"sockets not supported\"}}}};class l{fd_advise(e,r,n){return t}fd_allocate(e,r){return t}fd_close(){return 0}fd_datasync(){return t}fd_fdstat_get(){return{ret:t,fdstat:null}}fd_fdstat_set_flags(e){return t}fd_fdstat_set_rights(e,r){return t}fd_filestat_get(){return{ret:t,filestat:null}}fd_filestat_set_size(e){return t}fd_filestat_set_times(e,r,n){return t}fd_pread(e,r,n){return{ret:t,nread:0}}fd_prestat_get(){return{ret:t,prestat:null}}fd_prestat_dir_name(){return{ret:t,prestat_dir_name:null}}fd_pwrite(e,r,n){return{ret:t,nwritten:0}}fd_read(e,r){return{ret:t,nread:0}}fd_readdir_single(e){return{ret:t,dirent:null}}fd_seek(e,r){return{ret:t,offset:0n}}fd_sync(){return 0}fd_tell(){return{ret:t,offset:0n}}fd_write(e,r){return{ret:t,nwritten:0}}path_create_directory(e){return t}path_filestat_get(e,r){return{ret:t,filestat:null}}path_filestat_set_times(e,r,n,_,a){return t}path_link(e,r,n,_){return t}path_open(e,r,n,_,a,s){return{ret:t,fd_obj:null}}path_readlink(e){return{ret:t,data:null}}path_remove_directory(e){return t}path_rename(e,r,n){return t}path_symlink(e,r){return t}path_unlink_file(e){return t}}class p extends l{fd_fdstat_get(){return{ret:0,fdstat:new _(4,0)}}fd_read(e,t){let r=0;for(const n of t){if(!(this.file_pos<this.file.data.byteLength))break;{const t=this.file.data.slice(Number(this.file_pos),Number(this.file_pos+BigInt(n.buf_len)));e.set(t,n.buf),this.file_pos+=BigInt(t.length),r+=t.length}}return{ret:0,nread:r}}fd_pread(e,t,r){let n=0;for(const _ of t){if(!(r<this.file.data.byteLength))break;{const t=this.file.data.slice(Number(r),Number(r+BigInt(_.buf_len)));e.set(t,_.buf),r+=BigInt(t.length),n+=t.length}}return{ret:0,nread:n}}fd_seek(e,t){let r;switch(t){case 0:r=e;break;case 1:r=this.file_pos+e;break;case 2:r=BigInt(this.file.data.byteLength)+e;break;default:return{ret:28,offset:0n}}return r<0?{ret:28,offset:0n}:(this.file_pos=r,{ret:0,offset:this.file_pos})}fd_tell(){return{ret:0,offset:this.file_pos}}fd_write(e,t){let r=0;if(this.file.readonly)return{ret:8,nwritten:r};for(const n of t){const t=e.slice(n.buf,n.buf+n.buf_len);if(this.file_pos+BigInt(t.byteLength)>this.file.size){const e=this.file.data;this.file.data=new Uint8Array(Number(this.file_pos+BigInt(t.byteLength))),this.file.data.set(e)}this.file.data.set(t.slice(0,Number(this.file.size-this.file_pos)),Number(this.file_pos)),this.file_pos+=BigInt(t.byteLength),r+=n.buf_len}return{ret:0,nwritten:r}}fd_pwrite(e,t,r){let n=0;if(this.file.readonly)return{ret:8,nwritten:n};for(const _ of t){const t=e.slice(_.buf,_.buf+_.buf_len);if(r+BigInt(t.byteLength)>this.file.size){const e=this.file.data;this.file.data=new Uint8Array(Number(r+BigInt(t.byteLength))),this.file.data.set(e)}this.file.data.set(t.slice(0,Number(this.file.size-r)),Number(r)),r+=BigInt(t.byteLength),n+=_.buf_len}return{ret:0,nwritten:n}}fd_filestat_get(){return{ret:0,filestat:this.file.stat()}}constructor(e){super(),this.file_pos=0n,this.file=e}}class u{open(e){const t=new p(this);return 1&e&&t.fd_seek(0n,2),t}get size(){return BigInt(this.data.byteLength)}stat(){return new a(4,this.size)}truncate(){return this.readonly?63:(this.data=new Uint8Array([]),0)}constructor(e,t){this.data=new Uint8Array(e),this.readonly=!!t?.readonly}}class f{#e=0;#t=new Map;constructor(){}#r(){let e=this.#e;for(;;){if(!this.#t.has(e))return this.#e=e,e;e=e+1|0}}newJSVal(e){const t=this.#r();return this.#t.set(t,e),t}getJSVal(e){if(!this.#t.has(e))throw new WebAssembly.RuntimeError(`getJSVal(${e})`);return this.#t.get(e)}freeJSVal(e){if(!this.#t.delete(e))throw new WebAssembly.RuntimeError(`freeJSVal(${e})`)}}class d{#n=[];#_=new MessageChannel;constructor(){this.#_.port1.addEventListener(\"message\",(()=>{this.#n.pop()()})),this.#_.port1.start()}setImmediate(e,...t){this.#n.push((()=>e(...t))),this.#_.port2.postMessage(void 0)}}if(globalThis.setImmediate)o=globalThis.setImmediate;else{const e=new d;o=(t,...r)=>e.setImmediate(t,...r)}const E=e=>{const t=new f;return new FinalizationRegistry((t=>e.rts_freeStablePtr(t))),{newJSVal:e=>t.newJSVal(e),getJSVal:e=>t.getJSVal(e),freeJSVal:e=>t.freeJSVal(e),scheduleWork:()=>o(e.rts_schedulerLoop),ZC0ZChtmltzm0zi1zi0zi0zminplaceZCClickableziFFIZC:(t,r)=>{console.log(new TextDecoder(\"utf8\").decode(new Uint8Array(e.memory.buffer,t,r)))},ZC1ZChtmltzm0zi1zi0zi0zminplaceZCClickableziFFIZC:t=>function(e,t){const r=function(e,t){const r=new Uint8Array(e.memory.buffer,t),n=r[0]+(r[1]<<8)+(r[2]<<16)+(r[3]<<24)+(r[4]<<32)+(r[5]<<40)+(r[6]<<48)+(r[7]<<56),_=new Uint8Array(e.memory.buffer,t+8,n).slice().buffer;return new Uint8Array(_)}(e,t),n=g.sy.decode(r),_=(t,r)=>{const n=g.bR.encode(t),_=b(e,n);e.wasm_app(_)};switch(n.tag){case g.I4.EvalExpr:{const t=g.wL(_,[globalThis,null],null,n.expr),r={tag:g.dB.Return,0:g.bp(t)},a=g.bR.encode(r);return b(e,a)}case g.I4.HotReload:{window.location.reload();const t={tag:g.dB.Return,0:g.bp(null)},r=g.bR.encode(t);return b(e,r)}case g.I4.Halt:{const t={tag:g.dB.Return,0:g.bp(null)},r=g.bR.encode(t);return b(e,r)}}}(e,t)}};var g=__webpack_require__(933);function b(e,t){const r=t.byteLength,n=e.malloc(t.length+8);return new DataView(e.memory.buffer).setUint32(n,r,!0),new Uint8Array(e.memory.buffer,n+8,r).set(t),n}let x={};var h=__webpack_require__(366);const y=[window,null];window.startWasm=function(t){return e(this,arguments,void 0,(function*(e,t=null){const r=new c([],[],[new p(new u([])),new p(new u([])),new p(new u([]))]),n=yield WebAssembly.compileStreaming(fetch(e)),_=yield WebAssembly.instantiate(n,{wasi_snapshot_preview1:r.wasiImport,ghc_wasm_jsffi:E(x)});Object.assign(x,_.exports),r.initialize(_),yield _.exports.hs_init();const a=g.bp(t),s={tag:g.dB.Start,0:a},i=g.bR.encode(s),o=b(x,i);yield _.exports.wasm_app(o),window.addEventListener(\"beforeunload\",(()=>{const e={tag:g.dB.BeforeUnload},t=g.bR.encode(e),r=b(x,t);x.wasm_app(r)}))}))},window.startDev=function(t,r=null){const n=new WebSocket(t),_=(t,r)=>e(this,void 0,void 0,(function*(){n.send(g.bR.encode(t))}));n.onopen=e=>{const t=g.bp(r),_=g.bR.encode({tag:g.dB.Start,0:t});n.send(_)},n.onmessage=t=>e(this,void 0,void 0,(function*(){const r=yield(n=t.data,new Promise(((e,t)=>{const r=new FileReader;r.onload=()=>{const t=r.result,n=new Uint8Array(t);e(n)},r.onerror=e=>{t(e)},r.readAsArrayBuffer(n)})));var n;!function(t,r,n){e(this,void 0,void 0,(function*(){switch(t.tag){case g.I4.EvalExpr:{const e=g.wL(n,y,null,t.expr),r=g.bp(e);return n({tag:g.dB.Return,0:r},null)}case g.I4.HotReload:return void window.location.reload();case g.I4.Halt:return}(0,h.G)(t)}))}(g.sy.decode(r),0,_)})),n.onerror=e=>{console.error(\"WebSocket error:\",e)},n.onclose=e=>{console.log(\"WebSocket connection closed:\",e)}}})()})();"
