module Clickable.Main.Dev where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.State
import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.Function
import Data.IORef
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Tuple
import Data.Typeable
import Data.Word
import Foreign.Store
import GHC.Conc.Sync
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

import "this" Clickable.Internal (ClientMessage(..))
import "this" Clickable.Internal qualified as Internal
import "this" Clickable.Protocol
import "this" Clickable.Protocol.Value
import "this" Clickable.Types


data DevConfig a = DevConfig
  { aquire_resource :: IO a
  -- ^ Called once the GHCi session is loaded. Returns a polymorphic
  -- resource that typically contains a database connection and other
  -- long-lived entities that persist across versions when the
  -- application is reloaded.
  , release_resource :: a -> IO ()
  -- ^ Runs before the ghci session is unloaded
  , reload_app :: a -> IO ApplicationSpec
  -- ^ Given resource of type 'a', initialize instances of client and
  -- server applications. Runs each time ghci session reloads
  , html_template :: HtmlTemplateArgs -> Builder
  -- ^ Template for index.html, receives the current URL origin
  -- (protocol + host)
  , docroots :: [FilePath]
  -- ^ List of directories to use with wai-static middleware, could be
  -- empty, usually be used like docroots = ["./public"]
  , warp_settings :: Warp.Settings
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
  , warp_settings = Warp.defaultSettings
  }

runSettings :: Typeable resource => DevConfig resource -> IO ()
runSettings cfg = do
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
      forkIfRepl $ tryPort cfg.warp_settings $
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
      isRepl <- (== "<interactive>") <$> getProgName
      if isRepl then void (forkIO action) else action

runDev :: (ConnectionInfo -> StartFlags -> ClickM ()) -> IO ()
runDev clientApp = runSettings $ defaultConfig clientApp

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
      resp $ responseBuilder status200
        [(hContentType, "text/html; charset=utf-8")] $
        devserver_config.html_template $ HtmlTemplateArgs indexBundleJs devSocket

mkWebsocketUrl :: WAI.Request -> ByteString
mkWebsocketUrl req =
  WAI.requestHeaders req
    & List.lookup "Host"
    & fromMaybe "localhost"
    & ((if WAI.isSecure req then "wss://" else "ws://") <>)
    & (<> "/dev.sock")

data HtmlTemplateArgs = HtmlTemplateArgs
  { js_runtime :: ByteString
  , ws_url :: ByteString
  }

htmlTemplate :: HtmlTemplateArgs -> Builder
htmlTemplate hta =
  "<html>\n\
  \ <body>\n\
  \  <script>\n\
  \    " <> Builder.byteString hta.js_runtime <> "\n\
  \    clickable.startDev(\"" <> Builder.byteString hta.ws_url <> "\");\n\
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
      BrowserMessage (Return tid val) -> do
        mmvar <- atomicModifyIORef' conn.threads_ref $
          swap . Map.alterF (,Nothing) (fromIntegral tid.unInt32Le)
        forM_ mmvar \mvar ->
          void $ tryPutMVar mvar val
      BrowserMessage (TriggerEventMsg arg eid) -> void $ forkIO
        $ Internal.launchClickM conn.internal_env
        $ modify (Internal.triggerEvent (unsafeFromEventId eid) arg)
      BrowserMessage BeforeUnload ->
        return ()
      DevServerMessage a -> void $ forkIO
        $ Internal.launchClickM conn.internal_env a

    newConn connection = mdo
      command_chan <- newChan
      threads_ref <- newIORef Map.empty
      internal_env <- newInternalEnv connection threads_ref
      connInfo <- atomicModifyIORef' opt.conn_state_ref \m ->
        let
          connection_id = Map.lookupMax m & maybe 0 (succ . fst)
          connInfo = ConnectionInfo
            { internal_env
            , connection
            , command_chan
            , connection_id
            , threads_ref }
        in
          (Map.insert connection_id connInfo m, connInfo)
      return connInfo
    newInternalEnv :: Connection -> IORef (Map Word64 (MVar Value)) -> IO InternalEnv
    newInternalEnv conn awaiting =
      Internal.newInternalEnv \expr -> do
        tid <- fmap fromThreadId myThreadId
        sendDataMessage conn . Binary . Binary.encode $ EvalExpr (Int32Le (fromIntegral tid)) expr
        mvar <- newEmptyMVar
        modifyIORef' awaiting $ Map.insert tid mvar
        takeMVar mvar
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
  , threads_ref :: IORef (Map Word64 (MVar Value))
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
indexBundleJs = "(()=>{\"use strict\";var __webpack_modules__={924:(e,t,r)=>{r.d(t,{BN:()=>v,D:()=>C,Gk:()=>U,HA:()=>I,HQ:()=>A,In:()=>S,K2:()=>L,PV:()=>j,Sj:()=>B,Tf:()=>k,YE:()=>m,YO:()=>K,Yj:()=>R,b9:()=>W,g1:()=>V,l8:()=>w,tB:()=>H});var n=r(366);class _{encode(e){const t=h(this,e),r=new Uint8Array(t);return P(this,r,0,e),r}decode(e){const[t,r]=O(this,e,0);return t}}class a extends _{}class s extends _{}class i extends _{}class o extends _{}class l extends _{}class c extends _{}class u extends _{}class p extends _{}class d extends _{}class E extends _{}class f extends _{}class g extends _{}class b extends _{constructor(e){super(),this._element=e}}class D extends _{constructor(e){super(),this._description=e}}class T extends _{constructor(e){super(),this._alternatives=e}}class y extends _{constructor(e){super(),this._self=e}}class M extends _{constructor(e){super(),this._tuple=e}}function h(e,t){if(e instanceof a)return 1;if(e instanceof s)return 2;if(e instanceof i)return 4;if(e instanceof o)return 8;if(e instanceof l)return 1;if(e instanceof c)return 2;if(e instanceof u)return 4;if(e instanceof p)return 8;if(e instanceof d)return 4;if(e instanceof E)return 8;if(e instanceof g){const e=t;return 8+(new TextEncoder).encode(e).length}if(e instanceof f)return 8+t.length;if(e instanceof b){const r=8;return t.reduce(((t,r)=>t+h(e._element,r)),r)}if(e instanceof D){const r=t;return Object.keys(e._description).reduce(((t,n)=>t+h(e._description[n],r[n])),0)}if(e instanceof T){const r=t;return x(Object.keys(e._alternatives).length)+h(e._alternatives[r.tag],r)}if(e instanceof y)return h(e._self,t);if(e instanceof M){const r=t;return e._tuple.reduce(((e,t,n)=>e+h(t,r[n])),0)}return(0,n.G)(e)}function O(e,t,r){const _=new DataView(t.buffer);if(e instanceof a)return[_.getInt8(r),r+1];if(e instanceof s)return[_.getInt16(r,!0),r+2];if(e instanceof i)return[_.getInt32(r,!0),r+4];if(e instanceof o)return[_.getBigInt64(r,!0),r+8];if(e instanceof l)return[t[r],r+1];if(e instanceof c)return[_.getUint8(r),r+1];if(e instanceof u)return[_.getUint32(r,!0),r+4];if(e instanceof p)return[_.getBigUint64(r,!0),r+8];if(e instanceof d)return[_.getFloat32(r,!0),r+4];if(e instanceof E)return[_.getFloat64(r,!0),r+8];if(e instanceof g){const e=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),n=t.subarray(r+8,r+8+e);return[new TextDecoder(\"utf8\").decode(n),r+8+e]}if(e instanceof f){const e=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56);return[t.subarray(r+8,r+8+e),r+8+e]}if(e instanceof b){const n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),_=[];let a=r+8;for(let r=0;r<n;r++){const[r,n]=O(e._element,t,a);_.push(r),a=n}return[_,a]}if(e instanceof D){let n=r;return[Object.fromEntries(Object.entries(e._description).map((([e,r])=>{const[_,a]=O(r,t,n);return n=a,[e,_]}))),n]}if(e instanceof T){const n=x(Object.keys(e._alternatives).length),[_,a]=function(e,t,r){if(1!=e)throw new Error(\"Unimplemented\");return[t[r],r+1]}(n,t,r),[s,i]=O(e._alternatives[_],t,a);return s.tag=_,[s,i]}if(e instanceof y)return O(e._self,t,r);if(e instanceof M){let n=r;return[e._tuple.map((e=>{const[r,_]=O(e,t,n);return n=_,r})),n]}return(0,n.G)(e)}function P(e,t,r,_){const h=new DataView(t.buffer);if(e instanceof a)return h.setInt8(r,_),r+1;if(e instanceof s)return h.setInt16(r,_,!0),r+2;if(e instanceof i)return h.setInt32(r,_,!0),r+4;if(e instanceof o)return h.setBigInt64(r,_,!0),r+8;if(e instanceof l)return h.setUint8(r,_),r+1;if(e instanceof c)return h.setUint16(r,_,!0),r+2;if(e instanceof u)return h.setUint32(r,_,!0),r+4;if(e instanceof p)return h.setBigUint64(r,_,!0),r+8;if(e instanceof d)return h.setFloat32(r,_,!0),r+4;if(e instanceof E)return h.setFloat64(r,_,!0),r+8;if(e instanceof g){const e=_,n=(new TextEncoder).encode(e),a=n.length;return t[r+7]=255&a,t[r+6]=a>>8&255,t[r+5]=a>>16&255,t[r+4]=a>>24&255,t.set(n,r+8),r+8+a}if(e instanceof f){const e=_,n=e.length;return t[r+7]=255&n,t[r+6]=n>>8&255,t[r+5]=n>>16&255,t[r+4]=n>>24&255,t.set(e,r+8),r+8+n}if(e instanceof b){const n=_,a=n.length;t[r+7]=255&a,t[r+6]=a>>8&255,t[r+5]=a>>16&255,t[r+4]=a>>24&255;let s=r+8;for(let r=0;r<a;r++)s=P(e._element,t,s,n[r]);return s}if(e instanceof D){const n=_;let a=r;for(const r in e._description)Object.prototype.hasOwnProperty.call(e._description,r)&&(a=P(e._description[r],t,a,n[r]));return a}if(e instanceof T){const n=_.tag,a=x(Object.keys(e._alternatives).length);return t[r]=n,P(e._alternatives[n],t,r+a,_)}if(e instanceof y)return P(e._self,t,r,_);if(e instanceof M){const n=_;let a=r;return e._tuple.forEach(((e,r)=>{a=P(e,t,a,n[r])})),a}return(0,n.G)(e)}function x(e){return Math.ceil(Math.log2(e)/8)}const m=new a,w=new s,I=new i,U=new o,A=new l,B=new c,L=new u,C=new p,S=new d,v=new E,R=new g,W=new f;function K(e){return new b(e)}function V(e){return new D(e)}function k(e){return new T(e)}function j(...e){return new M(e)}function H(e){const t=new y(void 0),r=e(t);return t._self=r,r}var N;!function(e){e[e.Nothing=0]=\"Nothing\",e[e.Just=1]=\"Just\"}(N||(N={}))},366:(e,t,r)=>{function n(e){throw new Error(\"absurd: unreachable code\")}r.d(t,{G:()=>n,Y:()=>_});class _{constructor(){this.counter=0,this.map=new Map}push(e){const t=this.counter++;return this.map.set(t,e),t}set(e,t){this.map.set(e,t)}get(e){return this.map.get(e)}delete(e){return this.map.delete(e)}has(e){return this.map.has(e)}clear(){this.map.clear()}keys(){return this.map.keys()}values(){return this.map.values()}entries(){return this.map.entries()}forEach(e,t){this.map.forEach(e,t)}}},933:(__unused_webpack_module,__webpack_exports__,__webpack_require__)=>{__webpack_require__.d(__webpack_exports__,{Am:()=>evalUint8Array,I4:()=>HaskellMessageTag,bR:()=>javascriptMessage,bp:()=>unknownToValue,dB:()=>JavaScriptMessageTag,sy:()=>haskellMessage,wL:()=>evalExpr});var _binary__WEBPACK_IMPORTED_MODULE_0__=__webpack_require__(924),_lib__WEBPACK_IMPORTED_MODULE_1__=__webpack_require__(366),ValueTag;function Cons(e,t){return[e,t]}function car(e){return e[0]}function cdr(e){return e[1]}function evalExpr(e,t={}){return evalLoop(t.haskellCallback||(()=>{}),t.idenScope||globalContext,t.argScope||null,t.builderScope||null,e)}function evalUint8Array(e,t={}){return evalExpr(expr.decode(e),t)}function evalLoop(hscb,idenScope,argScope,builderScope,exp){var _a;switch(exp.tag){case ExprTag.Null:return null;case ExprTag.Boolean:return 0!=exp[0];case ExprTag.I8:case ExprTag.I16:case ExprTag.I32:case ExprTag.I64:case ExprTag.U8:case ExprTag.U16:case ExprTag.U32:case ExprTag.U64:case ExprTag.F32:case ExprTag.F64:case ExprTag.Str:return exp[0];case ExprTag.Arr:return exp[0].map(evalLoop.bind(void 0,hscb,idenScope,argScope,builderScope));case ExprTag.Obj:return Object.fromEntries(exp[0].map((([e,t])=>[e,evalLoop(hscb,idenScope,argScope,builderScope,t)])));case ExprTag.U8Arr:return exp[0];case ExprTag.Dot:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp[0]);return e[exp[1]]}case ExprTag.SetProp:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp[2]),t=evalLoop(hscb,idenScope,argScope,builderScope,exp[0]);return t[exp[1]]=e,e}case ExprTag.Ix:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.exp);return e[exp.ix]}case ExprTag.Plus:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp[0]),t=evalLoop(hscb,idenScope,argScope,builderScope,exp[1]);return e+t}case ExprTag.Subtract:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp[0]),t=evalLoop(hscb,idenScope,argScope,builderScope,exp[1]);return e-t}case ExprTag.Multiply:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp[0]),t=evalLoop(hscb,idenScope,argScope,builderScope,exp[1]);return e*t}case ExprTag.Divide:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp[0]),t=evalLoop(hscb,idenScope,argScope,builderScope,exp[1]);return e/t}case ExprTag.Id:{const e=exp[0];for(let t=idenScope;t;t=cdr(t)){const r=car(t);if(e in r)return r[e]}throw new Error(\"Variable not in scope: \"+exp[0])}case ExprTag.Lam:return function(){return evalLoop(hscb,idenScope,Cons(arguments,argScope),builderScope,exp.body)};case ExprTag.Arg:{let e=argScope,t=0;for(;e;){if(t==exp.scopeIx){const t=car(e);return t[exp.argIx]}e=cdr(e),t++}throw new Error(\"Argument scope out of a range: \"+exp.scopeIx)}case ExprTag.Apply:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp[0]);return e.apply(void 0,exp[1].map(evalLoop.bind(void 0,hscb,idenScope,argScope,builderScope)))}case ExprTag.Call:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp[0]),t=e[exp[1]];return t.apply(e,exp[2].map(evalLoop.bind(void 0,hscb,idenScope,argScope,builderScope)))}case ExprTag.AssignVar:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.rhs);if(varStorage.has(exp.scopeId)){const t=varStorage.get(exp.scopeId);t.set(exp.varId,e)}else{const t=new Map;t.set(exp.varId,e),varStorage.set(exp.scopeId,t)}return e}case ExprTag.FreeVar:{const e=varStorage.get(exp.scopeId);return e?(e.delete(exp.varId),0==e.size&&varStorage.delete(exp.scopeId),null):null}case ExprTag.Var:return null===(_a=varStorage.get(exp.scopeId))||void 0===_a?void 0:_a.get(exp.varId);case ExprTag.FreeScope:{varStorage.delete(exp.scopeId);const e=finalizers.get(exp.scopeId);return finalizers.delete(exp.scopeId),e&&e.forEach((e=>e())),null}case ExprTag.AskDomBuilder:if(null==builderScope)throw new Error(\"AskDomBuilder called without prior SupplyDomBuilder!\");return builderScope[0];case ExprTag.SupplyDomBuilder:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.builder),t=Cons(e,builderScope);return evalLoop(hscb,idenScope,argScope,t,exp.expr)}case ExprTag.InsertNode:{if(null==builderScope)throw new Error(\"InsertNode called without prior SupplyDomBuilder!\");const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.child);return domHelpers.insertIntoBuilder(builderScope[0],e),null}case ExprTag.ElementProp:{if(null==builderScope)throw new Error(\"ElementProp called without prior SupplyDomBuilder!\");const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.propValue);return domHelpers.assignProperty(builderScope[0],exp.propName,e),null}case ExprTag.ElementAttr:if(null==builderScope)throw new Error(\"ElementAttr called without prior SupplyDomBuilder!\");return domHelpers.assignAttribute(builderScope[0],exp.attrName,exp.attrValue),null;case ExprTag.ClassListAdd:{if(null==builderScope)throw new Error(\"InsertClassList called without prior SupplyDomBuilder!\");const e=domHelpers.domBuilderElement(builderScope[0]);return exp.classList.forEach((t=>e.classList.add(t))),null}case ExprTag.ClassListRemove:{if(null==builderScope)throw new Error(\"RemoveClassList called without prior SupplyDomBuilder!\");const e=domHelpers.domBuilderElement(builderScope[0]);return exp.classList.forEach((t=>e.classList.remove(t))),null}case ExprTag.InsertBrackets:if(null==builderScope)throw new Error(\"InsertBoundary called without prior SupplyDomBuilder!\");return domHelpers.insertBrackets(builderScope[0]);case ExprTag.ClearBrackets:if(null==builderScope)throw new Error(\"InsertBoundary called without prior SupplyDomBuilder!\");return domHelpers.clearBrackets(builderScope[0],Boolean(exp.detach));case ExprTag.CreateElement:return document.createElement(exp.tagName);case ExprTag.CreateElementNS:return document.createElementNS(exp.ns,exp.tagName);case ExprTag.CreateTextNode:return document.createTextNode(exp.content);case ExprTag.UpdateTextNode:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.node);return e.textContent=exp.content,null}case ExprTag.AddEventListener:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.target),t=evalLoop(hscb,idenScope,argScope,builderScope,exp.eventName),r=evalLoop(hscb,idenScope,argScope,builderScope,exp.listener);domHelpers.addEventListener(e,t,r);const n=finalizers.get(exp.reactiveScope),_=n||new _lib__WEBPACK_IMPORTED_MODULE_1__.Y;return n||finalizers.set(exp.reactiveScope,_),_.push((()=>domHelpers.removeEventListener(e,t,r)))}case ExprTag.ConnectResource:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.aquire),t=finalizers.get(exp.reactiveScope),r=t||new _lib__WEBPACK_IMPORTED_MODULE_1__.Y;return t||finalizers.set(exp.reactiveScope,r),r.push(e)}case ExprTag.SetTimeout:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.callback),t=finalizers.get(exp.reactiveScope),r=t||new _lib__WEBPACK_IMPORTED_MODULE_1__.Y;t||finalizers.set(exp.reactiveScope,r);let n=null;const _=r.push((()=>n&&clearTimeout(n)));return n=setTimeout((()=>{r.delete(_),n=null,e()}),exp.timeout),_}case ExprTag.ApplyFinalizer:{const e=finalizers.get(exp.reactiveScope),t=evalLoop(hscb,idenScope,argScope,builderScope,exp.finalizerId);if(!e)return!1;const r=e.get(t);return!!r&&(e.delete(t),r(),!0)}case ExprTag.RevSeq:return exp.exprs.reduceRight(((e,t)=>evalLoop(hscb,idenScope,argScope,builderScope,t)),null);case ExprTag.Eval:return eval(exp.rawJavaScript);case ExprTag.TriggerEvent:{const e=evalLoop(hscb,idenScope,argScope,builderScope,exp.arg),t={tag:JavaScriptMessageTag.TriggerEventMsg,arg:unknownToValue(e),callbackId:exp.callbackId};return hscb(t,argScope)}case ExprTag.UncaughtException:throw new Error(exp.message)}(0,_lib__WEBPACK_IMPORTED_MODULE_1__.G)(exp)}function unknownToValue(e){if(\"boolean\"==typeof e)return{tag:ValueTag.Vbool,0:e?1:0};if(\"number\"==typeof e)return Number.isInteger(e)?{tag:ValueTag.Vi32,0:e}:{tag:ValueTag.Vf64,0:e};if(\"string\"==typeof e)return{tag:ValueTag.Vstr,0:e};if(\"bigint\"==typeof e)return{tag:ValueTag.Vi64,0:e};if(Array.isArray(e))return{tag:ValueTag.Varr,0:e.map(unknownToValue)};if(e instanceof Uint8Array)return{tag:ValueTag.Vu8arr,0:e};if(null==e)return{tag:ValueTag.Vnull};const t=Object.entries(e).map((([e,t])=>[e,unknownToValue(t)]));return{tag:ValueTag.Vobj,0:t}}!function(e){e[e.Vnull=0]=\"Vnull\",e[e.Vbool=1]=\"Vbool\",e[e.Vi8=2]=\"Vi8\",e[e.Vi16=3]=\"Vi16\",e[e.Vi32=4]=\"Vi32\",e[e.Vi64=5]=\"Vi64\",e[e.Vu8=6]=\"Vu8\",e[e.Vu16=7]=\"Vu16\",e[e.Vu32=8]=\"Vu32\",e[e.Vu64=9]=\"Vu64\",e[e.Vf32=10]=\"Vf32\",e[e.Vf64=11]=\"Vf64\",e[e.Vstr=12]=\"Vstr\",e[e.Varr=13]=\"Varr\",e[e.Vobj=14]=\"Vobj\",e[e.Vu8arr=15]=\"Vu8arr\"}(ValueTag||(ValueTag={}));const jvalue=_binary__WEBPACK_IMPORTED_MODULE_0__.tB((e=>_binary__WEBPACK_IMPORTED_MODULE_0__.Tf({[ValueTag.Vnull]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({}),[ValueTag.Vbool]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ValueTag.Vi8]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ValueTag.Vi16]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.l8}),[ValueTag.Vi32]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ValueTag.Vi64]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Gk}),[ValueTag.Vu8]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.HQ}),[ValueTag.Vu16]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Sj}),[ValueTag.Vu32]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.K2}),[ValueTag.Vu64]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.D}),[ValueTag.Vf32]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.In}),[ValueTag.Vf64]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.BN}),[ValueTag.Vstr]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ValueTag.Varr]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ValueTag.Vobj]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(_binary__WEBPACK_IMPORTED_MODULE_0__.PV(_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,e))}),[ValueTag.Vu8arr]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.b9})})));var ExprTag;!function(e){e[e.Null=0]=\"Null\",e[e.Boolean=1]=\"Boolean\",e[e.I8=2]=\"I8\",e[e.I16=3]=\"I16\",e[e.I32=4]=\"I32\",e[e.I64=5]=\"I64\",e[e.U8=6]=\"U8\",e[e.U16=7]=\"U16\",e[e.U32=8]=\"U32\",e[e.U64=9]=\"U64\",e[e.F32=10]=\"F32\",e[e.F64=11]=\"F64\",e[e.Str=12]=\"Str\",e[e.Arr=13]=\"Arr\",e[e.Obj=14]=\"Obj\",e[e.U8Arr=15]=\"U8Arr\",e[e.Dot=16]=\"Dot\",e[e.SetProp=17]=\"SetProp\",e[e.Ix=18]=\"Ix\",e[e.Plus=19]=\"Plus\",e[e.Subtract=20]=\"Subtract\",e[e.Multiply=21]=\"Multiply\",e[e.Divide=22]=\"Divide\",e[e.Id=23]=\"Id\",e[e.Lam=24]=\"Lam\",e[e.Arg=25]=\"Arg\",e[e.Apply=26]=\"Apply\",e[e.Call=27]=\"Call\",e[e.AssignVar=28]=\"AssignVar\",e[e.FreeVar=29]=\"FreeVar\",e[e.Var=30]=\"Var\",e[e.FreeScope=31]=\"FreeScope\",e[e.AskDomBuilder=32]=\"AskDomBuilder\",e[e.SupplyDomBuilder=33]=\"SupplyDomBuilder\",e[e.InsertNode=34]=\"InsertNode\",e[e.ElementProp=35]=\"ElementProp\",e[e.ElementAttr=36]=\"ElementAttr\",e[e.ClassListAdd=37]=\"ClassListAdd\",e[e.ClassListRemove=38]=\"ClassListRemove\",e[e.InsertBrackets=39]=\"InsertBrackets\",e[e.ClearBrackets=40]=\"ClearBrackets\",e[e.CreateElement=41]=\"CreateElement\",e[e.CreateElementNS=42]=\"CreateElementNS\",e[e.CreateTextNode=43]=\"CreateTextNode\",e[e.UpdateTextNode=44]=\"UpdateTextNode\",e[e.AddEventListener=45]=\"AddEventListener\",e[e.ConnectResource=46]=\"ConnectResource\",e[e.SetTimeout=47]=\"SetTimeout\",e[e.ApplyFinalizer=48]=\"ApplyFinalizer\",e[e.RevSeq=49]=\"RevSeq\",e[e.Eval=50]=\"Eval\",e[e.TriggerEvent=51]=\"TriggerEvent\",e[e.UncaughtException=52]=\"UncaughtException\"}(ExprTag||(ExprTag={}));const expr=_binary__WEBPACK_IMPORTED_MODULE_0__.tB((e=>_binary__WEBPACK_IMPORTED_MODULE_0__.Tf({[ExprTag.Null]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({}),[ExprTag.Boolean]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ExprTag.I8]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ExprTag.I16]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.l8}),[ExprTag.I32]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.I64]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Gk}),[ExprTag.U8]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.HQ}),[ExprTag.U16]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Sj}),[ExprTag.U32]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.K2}),[ExprTag.U64]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.D}),[ExprTag.F32]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.In}),[ExprTag.F64]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.BN}),[ExprTag.Str]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.Arr]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ExprTag.Obj]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(_binary__WEBPACK_IMPORTED_MODULE_0__.PV(_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,e))}),[ExprTag.U8Arr]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.b9}),[ExprTag.Dot]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.SetProp]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,2:e}),[ExprTag.Ix]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({exp:e,ix:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.Plus]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:e}),[ExprTag.Subtract]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:e}),[ExprTag.Multiply]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:e}),[ExprTag.Divide]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:e}),[ExprTag.Id]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.Lam]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({body:e}),[ExprTag.Arg]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeIx:_binary__WEBPACK_IMPORTED_MODULE_0__.YE,argIx:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ExprTag.Apply]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ExprTag.Call]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,2:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ExprTag.AssignVar]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,varId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,rhs:e}),[ExprTag.FreeVar]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,varId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.Var]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,varId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.FreeScope]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({scopeId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.AskDomBuilder]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({}),[ExprTag.SupplyDomBuilder]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({builder:e,expr:e}),[ExprTag.InsertNode]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({child:e}),[ExprTag.ElementProp]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({propName:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,propValue:e}),[ExprTag.ElementAttr]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({attrName:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,attrValue:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.ClassListAdd]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({classList:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(_binary__WEBPACK_IMPORTED_MODULE_0__.Yj)}),[ExprTag.ClassListRemove]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({classList:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(_binary__WEBPACK_IMPORTED_MODULE_0__.Yj)}),[ExprTag.InsertBrackets]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({}),[ExprTag.ClearBrackets]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({detach:_binary__WEBPACK_IMPORTED_MODULE_0__.YE}),[ExprTag.CreateElement]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({tagName:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.CreateElementNS]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({ns:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj,tagName:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.CreateTextNode]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({content:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.UpdateTextNode]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({node:e,content:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.AddEventListener]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({reactiveScope:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,target:e,eventName:e,listener:e}),[ExprTag.ConnectResource]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({reactiveScope:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,aquire:e}),[ExprTag.SetTimeout]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({reactiveScope:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,callback:e,timeout:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[ExprTag.ApplyFinalizer]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({reactiveScope:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,finalizerId:e}),[ExprTag.RevSeq]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({exprs:_binary__WEBPACK_IMPORTED_MODULE_0__.YO(e)}),[ExprTag.Eval]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({rawJavaScript:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj}),[ExprTag.TriggerEvent]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({callbackId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,arg:e}),[ExprTag.UncaughtException]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({message:_binary__WEBPACK_IMPORTED_MODULE_0__.Yj})})));var HaskellMessageTag;!function(e){e[e.EvalExpr=0]=\"EvalExpr\",e[e.HotReload=1]=\"HotReload\",e[e.Halt=2]=\"Halt\"}(HaskellMessageTag||(HaskellMessageTag={}));const haskellMessage=_binary__WEBPACK_IMPORTED_MODULE_0__.Tf({[HaskellMessageTag.EvalExpr]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({threadId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,expr}),[HaskellMessageTag.HotReload]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({}),[HaskellMessageTag.Halt]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({})});var JavaScriptMessageTag;!function(e){e[e.Start=0]=\"Start\",e[e.Return=1]=\"Return\",e[e.TriggerEventMsg=2]=\"TriggerEventMsg\",e[e.BeforeUnload=3]=\"BeforeUnload\"}(JavaScriptMessageTag||(JavaScriptMessageTag={}));const javascriptMessage=_binary__WEBPACK_IMPORTED_MODULE_0__.Tf({[JavaScriptMessageTag.Start]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({0:jvalue}),[JavaScriptMessageTag.Return]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({threadId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA,value:jvalue}),[JavaScriptMessageTag.TriggerEventMsg]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({arg:jvalue,callbackId:_binary__WEBPACK_IMPORTED_MODULE_0__.HA}),[JavaScriptMessageTag.BeforeUnload]:_binary__WEBPACK_IMPORTED_MODULE_0__.g1({})}),varStorage=new Map,finalizers=new Map,globalContext=[window,null];var domHelpers;!function(e){function t(e,t){e instanceof Comment?e.parentElement.insertBefore(t,e):e.appendChild(t)}function r(e){return e instanceof Comment?e.parentElement:e}function n(e){return e instanceof Comment&&\"ContentBrackets {{\"==e.textContent}e.insertIntoBuilder=t,e.assignProperty=function(e,t,r){e instanceof Comment?e.parentElement[t]=r:e[t]=r},e.assignAttribute=function(e,t,n){r(e).setAttribute(t,n)},e.addEventListener=function(e,t,n){r(e).addEventListener(t,n)},e.removeEventListener=function(e,t,n){r(e).removeEventListener(t,n)},e.insertBrackets=function(e){const r=document.createComment(\"ContentBrackets {{\"),n=document.createComment(\"}}\");return t(e,r),t(e,n),n},e.clearBrackets=function(e,t){if(e instanceof Comment){let _=0;for(;e.previousSibling&&(0!=_||!n(e.previousSibling));)(r=e.previousSibling)instanceof Comment&&\"}}\"==r.textContent?_++:n(e.previousSibling)&&_--,e.previousSibling.parentNode.removeChild(e.previousSibling);t&&(e.previousSibling.parentNode.removeChild(e.previousSibling),e.parentNode.removeChild(e))}else e.innerHTML=\"\";var r},e.domBuilderElement=r}(domHelpers||(domHelpers={}))}},__webpack_module_cache__={};function __webpack_require__(e){var t=__webpack_module_cache__[e];if(void 0!==t)return t.exports;var r=__webpack_module_cache__[e]={exports:{}};return __webpack_modules__[e](r,r.exports,__webpack_require__),r.exports}__webpack_require__.d=(e,t)=>{for(var r in t)__webpack_require__.o(t,r)&&!__webpack_require__.o(e,r)&&Object.defineProperty(e,r,{enumerable:!0,get:t[r]})},__webpack_require__.o=(e,t)=>Object.prototype.hasOwnProperty.call(e,t);var __webpack_exports__={};(()=>{function e(e,t,r,n){return new(r||(r=Promise))((function(_,a){function s(e){try{o(n.next(e))}catch(e){a(e)}}function i(e){try{o(n.throw(e))}catch(e){a(e)}}function o(e){var t;e.done?_(e.value):(t=e.value,t instanceof r?t:new r((function(e){e(t)}))).then(s,i)}o((n=n.apply(e,t||[])).next())}))}var t=__webpack_require__(366),r=__webpack_require__(933);const n=58;class _{static read_bytes(e,t){const r=new _;return r.buf=e.getUint32(t,!0),r.buf_len=e.getUint32(t+4,!0),r}static read_bytes_array(e,t,r){const n=[];for(let a=0;a<r;a++)n.push(_.read_bytes(e,t+8*a));return n}}class a{static read_bytes(e,t){const r=new a;return r.buf=e.getUint32(t,!0),r.buf_len=e.getUint32(t+4,!0),r}static read_bytes_array(e,t,r){const n=[];for(let _=0;_<r;_++)n.push(a.read_bytes(e,t+8*_));return n}}class s{write_bytes(e,t){e.setUint8(t,this.fs_filetype),e.setUint16(t+2,this.fs_flags,!0),e.setBigUint64(t+8,this.fs_rights_base,!0),e.setBigUint64(t+16,this.fs_rights_inherited,!0)}constructor(e,t){this.fs_rights_base=0n,this.fs_rights_inherited=0n,this.fs_filetype=e,this.fs_flags=t}}class i{write_bytes(e,t){e.setBigUint64(t,this.dev,!0),e.setBigUint64(t+8,this.ino,!0),e.setUint8(t+16,this.filetype),e.setBigUint64(t+24,this.nlink,!0),e.setBigUint64(t+32,this.size,!0),e.setBigUint64(t+38,this.atim,!0),e.setBigUint64(t+46,this.mtim,!0),e.setBigUint64(t+52,this.ctim,!0)}constructor(e,t){this.dev=0n,this.ino=0n,this.nlink=0n,this.atim=0n,this.mtim=0n,this.ctim=0n,this.filetype=e,this.size=t}}const o=new class{enable(e){this.log=function(e,t){return e?console.log.bind(console,\"%c%s\",\"color: #265BA0\",t):()=>{}}(void 0===e||e,this.prefix)}get enabled(){return this.isEnabled}constructor(e){this.isEnabled=e,this.prefix=\"wasi:\",this.enable(e)}}(!1);class l extends Error{constructor(e){super(\"exit with exit code \"+e),this.code=e}}let c,u=class{start(e){this.inst=e;try{return e.exports._start(),0}catch(e){if(e instanceof l)return e.code;throw e}}initialize(e){this.inst=e,e.exports._initialize()}constructor(e,t,r,n={}){this.args=[],this.env=[],this.fds=[],o.enable(n.debug),this.args=e,this.env=t,this.fds=r;const s=this;this.wasiImport={args_sizes_get(e,t){const r=new DataView(s.inst.exports.memory.buffer);r.setUint32(e,s.args.length,!0);let n=0;for(const e of s.args)n+=e.length+1;return r.setUint32(t,n,!0),o.log(r.getUint32(e,!0),r.getUint32(t,!0)),0},args_get(e,t){const r=new DataView(s.inst.exports.memory.buffer),n=new Uint8Array(s.inst.exports.memory.buffer),_=t;for(let _=0;_<s.args.length;_++){r.setUint32(e,t,!0),e+=4;const a=(new TextEncoder).encode(s.args[_]);n.set(a,t),r.setUint8(t+a.length,0),t+=a.length+1}return o.enabled&&o.log(new TextDecoder(\"utf-8\").decode(n.slice(_,t))),0},environ_sizes_get(e,t){const r=new DataView(s.inst.exports.memory.buffer);r.setUint32(e,s.env.length,!0);let n=0;for(const e of s.env)n+=e.length+1;return r.setUint32(t,n,!0),o.log(r.getUint32(e,!0),r.getUint32(t,!0)),0},environ_get(e,t){const r=new DataView(s.inst.exports.memory.buffer),n=new Uint8Array(s.inst.exports.memory.buffer),_=t;for(let _=0;_<s.env.length;_++){r.setUint32(e,t,!0),e+=4;const a=(new TextEncoder).encode(s.env[_]);n.set(a,t),r.setUint8(t+a.length,0),t+=a.length+1}return o.enabled&&o.log(new TextDecoder(\"utf-8\").decode(n.slice(_,t))),0},clock_res_get(e,t){let r;switch(e){case 1:r=5000n;break;case 0:r=1000000n;break;default:return 52}return new DataView(s.inst.exports.memory.buffer).setBigUint64(t,r,!0),0},clock_time_get(e,t,r){const n=new DataView(s.inst.exports.memory.buffer);if(0===e)n.setBigUint64(r,1000000n*BigInt((new Date).getTime()),!0);else if(1==e){let e;try{e=BigInt(Math.round(1e6*performance.now()))}catch(t){e=0n}n.setBigUint64(r,e,!0)}else n.setBigUint64(r,0n,!0);return 0},fd_advise:(e,t,r,n)=>null!=s.fds[e]?s.fds[e].fd_advise(t,r,n):8,fd_allocate:(e,t,r)=>null!=s.fds[e]?s.fds[e].fd_allocate(t,r):8,fd_close(e){if(null!=s.fds[e]){const t=s.fds[e].fd_close();return s.fds[e]=void 0,t}return 8},fd_datasync:e=>null!=s.fds[e]?s.fds[e].fd_datasync():8,fd_fdstat_get(e,t){if(null!=s.fds[e]){const{ret:r,fdstat:n}=s.fds[e].fd_fdstat_get();return null!=n&&n.write_bytes(new DataView(s.inst.exports.memory.buffer),t),r}return 8},fd_fdstat_set_flags:(e,t)=>null!=s.fds[e]?s.fds[e].fd_fdstat_set_flags(t):8,fd_fdstat_set_rights:(e,t,r)=>null!=s.fds[e]?s.fds[e].fd_fdstat_set_rights(t,r):8,fd_filestat_get(e,t){if(null!=s.fds[e]){const{ret:r,filestat:n}=s.fds[e].fd_filestat_get();return null!=n&&n.write_bytes(new DataView(s.inst.exports.memory.buffer),t),r}return 8},fd_filestat_set_size:(e,t)=>null!=s.fds[e]?s.fds[e].fd_filestat_set_size(t):8,fd_filestat_set_times:(e,t,r,n)=>null!=s.fds[e]?s.fds[e].fd_filestat_set_times(t,r,n):8,fd_pread(e,t,r,n,a){const i=new DataView(s.inst.exports.memory.buffer),o=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const l=_.read_bytes_array(i,t,r),{ret:c,nread:u}=s.fds[e].fd_pread(o,l,n);return i.setUint32(a,u,!0),c}return 8},fd_prestat_get(e,t){const r=new DataView(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const{ret:n,prestat:_}=s.fds[e].fd_prestat_get();return null!=_&&_.write_bytes(r,t),n}return 8},fd_prestat_dir_name(e,t,r){if(null!=s.fds[e]){const{ret:r,prestat_dir_name:n}=s.fds[e].fd_prestat_dir_name();return null!=n&&new Uint8Array(s.inst.exports.memory.buffer).set(n,t),r}return 8},fd_pwrite(e,t,r,n,_){const i=new DataView(s.inst.exports.memory.buffer),o=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const l=a.read_bytes_array(i,t,r),{ret:c,nwritten:u}=s.fds[e].fd_pwrite(o,l,n);return i.setUint32(_,u,!0),c}return 8},fd_read(e,t,r,n){const a=new DataView(s.inst.exports.memory.buffer),i=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const o=_.read_bytes_array(a,t,r),{ret:l,nread:c}=s.fds[e].fd_read(i,o);return a.setUint32(n,c,!0),l}return 8},fd_readdir(e,t,r,n,_){const a=new DataView(s.inst.exports.memory.buffer),i=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){let o=0;for(;;){const{ret:l,dirent:c}=s.fds[e].fd_readdir_single(n);if(0!=l)return a.setUint32(_,o,!0),l;if(null==c)break;if(r-o<c.head_length()){o=r;break}const u=new ArrayBuffer(c.head_length());if(c.write_head_bytes(new DataView(u),0),i.set(new Uint8Array(u).slice(0,Math.min(u.byteLength,r-o)),t),t+=c.head_length(),o+=c.head_length(),r-o<c.name_length()){o=r;break}c.write_name_bytes(i,t,r-o),t+=c.name_length(),o+=c.name_length(),n=c.d_next}return a.setUint32(_,o,!0),0}return 8},fd_renumber(e,t){if(null!=s.fds[e]&&null!=s.fds[t]){const r=s.fds[t].fd_close();return 0!=r?r:(s.fds[t]=s.fds[e],s.fds[e]=void 0,0)}return 8},fd_seek(e,t,r,n){const _=new DataView(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const{ret:a,offset:i}=s.fds[e].fd_seek(t,r);return _.setBigInt64(n,i,!0),a}return 8},fd_sync:e=>null!=s.fds[e]?s.fds[e].fd_sync():8,fd_tell(e,t){const r=new DataView(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const{ret:n,offset:_}=s.fds[e].fd_tell();return r.setBigUint64(t,_,!0),n}return 8},fd_write(e,t,r,n){const _=new DataView(s.inst.exports.memory.buffer),i=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const o=a.read_bytes_array(_,t,r),{ret:l,nwritten:c}=s.fds[e].fd_write(i,o);return _.setUint32(n,c,!0),l}return 8},path_create_directory(e,t,r){const n=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const _=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return s.fds[e].path_create_directory(_)}},path_filestat_get(e,t,r,n,_){const a=new DataView(s.inst.exports.memory.buffer),i=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const o=new TextDecoder(\"utf-8\").decode(i.slice(r,r+n)),{ret:l,filestat:c}=s.fds[e].path_filestat_get(t,o);return null!=c&&c.write_bytes(a,_),l}return 8},path_filestat_set_times(e,t,r,n,_,a,i){const o=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const l=new TextDecoder(\"utf-8\").decode(o.slice(r,r+n));return s.fds[e].path_filestat_set_times(t,l,_,a,i)}return 8},path_link(e,t,r,n,_,a,i){const o=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]&&null!=s.fds[_]){const l=new TextDecoder(\"utf-8\").decode(o.slice(r,r+n)),c=new TextDecoder(\"utf-8\").decode(o.slice(a,a+i));return s.fds[_].path_link(e,t,l,c)}return 8},path_open(e,t,r,n,_,a,i,l,c){const u=new DataView(s.inst.exports.memory.buffer),p=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const d=new TextDecoder(\"utf-8\").decode(p.slice(r,r+n));o.log(d);const{ret:E,fd_obj:f}=s.fds[e].path_open(t,d,_,a,i,l);if(0!=E)return E;s.fds.push(f);const g=s.fds.length-1;return u.setUint32(c,g,!0),0}return 8},path_readlink(e,t,r,n,_,a){const i=new DataView(s.inst.exports.memory.buffer),l=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const c=new TextDecoder(\"utf-8\").decode(l.slice(t,t+r));o.log(c);const{ret:u,data:p}=s.fds[e].path_readlink(c);if(null!=p){const e=(new TextEncoder).encode(p);if(e.length>_)return i.setUint32(a,0,!0),8;l.set(e,n),i.setUint32(a,e.length,!0)}return u}return 8},path_remove_directory(e,t,r){const n=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const _=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return s.fds[e].path_remove_directory(_)}return 8},path_rename(e,t,r,n,_,a){throw\"FIXME what is the best abstraction for this?\"},path_symlink(e,t,r,n,_){const a=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[r]){const i=new TextDecoder(\"utf-8\").decode(a.slice(e,e+t)),o=new TextDecoder(\"utf-8\").decode(a.slice(n,n+_));return s.fds[r].path_symlink(i,o)}return 8},path_unlink_file(e,t,r){const n=new Uint8Array(s.inst.exports.memory.buffer);if(null!=s.fds[e]){const _=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return s.fds[e].path_unlink_file(_)}return 8},poll_oneoff(e,t,r){throw\"async io not supported\"},proc_exit(e){throw new l(e)},proc_raise(e){throw\"raised signal \"+e},sched_yield(){},random_get(e,t){const r=new Uint8Array(s.inst.exports.memory.buffer);for(let n=0;n<t;n++)r[e+n]=256*Math.random()|0},sock_recv(e,t,r){throw\"sockets not supported\"},sock_send(e,t,r){throw\"sockets not supported\"},sock_shutdown(e,t){throw\"sockets not supported\"},sock_accept(e,t){throw\"sockets not supported\"}}}};class p{fd_advise(e,t,r){return n}fd_allocate(e,t){return n}fd_close(){return 0}fd_datasync(){return n}fd_fdstat_get(){return{ret:n,fdstat:null}}fd_fdstat_set_flags(e){return n}fd_fdstat_set_rights(e,t){return n}fd_filestat_get(){return{ret:n,filestat:null}}fd_filestat_set_size(e){return n}fd_filestat_set_times(e,t,r){return n}fd_pread(e,t,r){return{ret:n,nread:0}}fd_prestat_get(){return{ret:n,prestat:null}}fd_prestat_dir_name(){return{ret:n,prestat_dir_name:null}}fd_pwrite(e,t,r){return{ret:n,nwritten:0}}fd_read(e,t){return{ret:n,nread:0}}fd_readdir_single(e){return{ret:n,dirent:null}}fd_seek(e,t){return{ret:n,offset:0n}}fd_sync(){return 0}fd_tell(){return{ret:n,offset:0n}}fd_write(e,t){return{ret:n,nwritten:0}}path_create_directory(e){return n}path_filestat_get(e,t){return{ret:n,filestat:null}}path_filestat_set_times(e,t,r,_,a){return n}path_link(e,t,r,_){return n}path_open(e,t,r,_,a,s){return{ret:n,fd_obj:null}}path_readlink(e){return{ret:n,data:null}}path_remove_directory(e){return n}path_rename(e,t,r){return n}path_symlink(e,t){return n}path_unlink_file(e){return n}}class d extends p{fd_fdstat_get(){return{ret:0,fdstat:new s(4,0)}}fd_read(e,t){let r=0;for(const n of t){if(!(this.file_pos<this.file.data.byteLength))break;{const t=this.file.data.slice(Number(this.file_pos),Number(this.file_pos+BigInt(n.buf_len)));e.set(t,n.buf),this.file_pos+=BigInt(t.length),r+=t.length}}return{ret:0,nread:r}}fd_pread(e,t,r){let n=0;for(const _ of t){if(!(r<this.file.data.byteLength))break;{const t=this.file.data.slice(Number(r),Number(r+BigInt(_.buf_len)));e.set(t,_.buf),r+=BigInt(t.length),n+=t.length}}return{ret:0,nread:n}}fd_seek(e,t){let r;switch(t){case 0:r=e;break;case 1:r=this.file_pos+e;break;case 2:r=BigInt(this.file.data.byteLength)+e;break;default:return{ret:28,offset:0n}}return r<0?{ret:28,offset:0n}:(this.file_pos=r,{ret:0,offset:this.file_pos})}fd_tell(){return{ret:0,offset:this.file_pos}}fd_write(e,t){let r=0;if(this.file.readonly)return{ret:8,nwritten:r};for(const n of t){const t=e.slice(n.buf,n.buf+n.buf_len);if(this.file_pos+BigInt(t.byteLength)>this.file.size){const e=this.file.data;this.file.data=new Uint8Array(Number(this.file_pos+BigInt(t.byteLength))),this.file.data.set(e)}this.file.data.set(t.slice(0,Number(this.file.size-this.file_pos)),Number(this.file_pos)),this.file_pos+=BigInt(t.byteLength),r+=n.buf_len}return{ret:0,nwritten:r}}fd_pwrite(e,t,r){let n=0;if(this.file.readonly)return{ret:8,nwritten:n};for(const _ of t){const t=e.slice(_.buf,_.buf+_.buf_len);if(r+BigInt(t.byteLength)>this.file.size){const e=this.file.data;this.file.data=new Uint8Array(Number(r+BigInt(t.byteLength))),this.file.data.set(e)}this.file.data.set(t.slice(0,Number(this.file.size-r)),Number(r)),r+=BigInt(t.byteLength),n+=_.buf_len}return{ret:0,nwritten:n}}fd_filestat_get(){return{ret:0,filestat:this.file.stat()}}constructor(e){super(),this.file_pos=0n,this.file=e}}class E{open(e){const t=new d(this);return 1&e&&t.fd_seek(0n,2),t}get size(){return BigInt(this.data.byteLength)}stat(){return new i(4,this.size)}truncate(){return this.readonly?63:(this.data=new Uint8Array([]),0)}constructor(e,t){this.data=new Uint8Array(e),this.readonly=!!t?.readonly}}class f{#e=0;#t=new Map;constructor(){}#r(){let e=this.#e;for(;;){if(!this.#t.has(e))return this.#e=e,e;e=e+1|0}}newJSVal(e){const t=this.#r();return this.#t.set(t,e),t}getJSVal(e){if(!this.#t.has(e))throw new WebAssembly.RuntimeError(`getJSVal(${e})`);return this.#t.get(e)}freeJSVal(e){if(!this.#t.delete(e))throw new WebAssembly.RuntimeError(`freeJSVal(${e})`)}}class g{#n=[];#_=new MessageChannel;constructor(){this.#_.port1.addEventListener(\"message\",(()=>{this.#n.pop()()})),this.#_.port1.start()}setImmediate(e,...t){this.#n.push((()=>e(...t))),this.#_.port2.postMessage(void 0)}}if(globalThis.setImmediate)c=globalThis.setImmediate;else{const e=new g;c=(t,...r)=>e.setImmediate(t,...r)}const b=e=>{const t=new f;return new FinalizationRegistry((t=>e.rts_freeStablePtr(t))),{newJSVal:e=>t.newJSVal(e),getJSVal:e=>t.getJSVal(e),freeJSVal:e=>t.freeJSVal(e),scheduleWork:()=>c(e.rts_schedulerLoop),ZC0ZChtmltzm0zi1zi0zi0zminplaceZCClickableziFFIZC:(t,r)=>{console.log(new TextDecoder(\"utf8\").decode(new Uint8Array(e.memory.buffer,t,r)))},ZC1ZChtmltzm0zi1zi0zi0zminplaceZCClickableziFFIZC:t=>function(e,t){const n=function(e,t){const r=new Uint8Array(e.memory.buffer,t),n=r[0]+(r[1]<<8)+(r[2]<<16)+(r[3]<<24)+(r[4]<<32)+(r[5]<<40)+(r[6]<<48)+(r[7]<<56),_=new Uint8Array(e.memory.buffer,t+8,n).slice().buffer;return new Uint8Array(_)}(e,t),_=r.sy.decode(n),a=t=>{const n=r.bR.encode(t),_=D(e,n);e.wasm_app(_)};switch(_.tag){case r.I4.EvalExpr:{const t=r.wL(_.expr,{haskellCallback:a}),n={tag:r.dB.Return,value:r.bp(t),threadId:_.threadId},s=r.bR.encode(n);return D(e,s)}case r.I4.HotReload:return window.location.reload(),0;case r.I4.Halt:return 0}}(e,t)}};function D(e,t){const r=t.byteLength,n=e.malloc(t.length+8);return new DataView(e.memory.buffer).setUint32(n,r,!0),new Uint8Array(e.memory.buffer,n+8,r).set(t),n}let T={};window.clickable={startWasm:function(t){return e(this,arguments,void 0,(function*(e,t=null){const n=new u([],[],[new d(new E([])),new d(new E([])),new d(new E([]))]),_=yield WebAssembly.compileStreaming(fetch(e)),a=yield WebAssembly.instantiate(_,{wasi_snapshot_preview1:n.wasiImport,ghc_wasm_jsffi:b(T)});Object.assign(T,a.exports),n.initialize(a),yield a.exports.hs_init();const s=r.bp(t),i={tag:r.dB.Start,0:s},o=r.bR.encode(i),l=D(T,o);yield a.exports.wasm_app(l),window.addEventListener(\"beforeunload\",(()=>{const e={tag:r.dB.BeforeUnload},t=r.bR.encode(e),n=D(T,t);T.wasm_app(n)}))}))},startDev:function(n,_=null){const a=new WebSocket(n),s=t=>e(this,void 0,void 0,(function*(){a.send(r.bR.encode(t))}));a.onopen=e=>{const t=r.bp(_),n=r.bR.encode({tag:r.dB.Start,0:t});a.send(n)},a.onmessage=n=>e(this,void 0,void 0,(function*(){const _=yield(a=n.data,new Promise(((e,t)=>{const r=new FileReader;r.onload=()=>{const t=r.result,n=new Uint8Array(t);e(n)},r.onerror=e=>{t(e)},r.readAsArrayBuffer(a)})));var a;!function(n,_){e(this,void 0,void 0,(function*(){switch(n.tag){case r.I4.EvalExpr:{const e=r.wL(n.expr,{haskellCallback:_}),t=r.bp(e),a={tag:r.dB.Return,value:t,threadId:n.threadId};return _(a)}case r.I4.HotReload:return void window.location.reload();case r.I4.Halt:return}(0,t.G)(n)}))}(r.sy.decode(_),s)})),a.onerror=e=>{console.error(\"WebSocket error:\",e)},a.onclose=e=>{console.log(\"WebSocket connection closed, reloading the tab…\"),function e(t){const r=new WebSocket(n),_=Math.min(3e4,2*t);r.onopen=e=>window.location.reload(),r.onclose=r=>{setTimeout((()=>e(_)),t)}}(100)}},evalExpr:r.wL,evalUint8Array:r.Am}})()})();"
