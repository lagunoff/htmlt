module Clickable.Protocol where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Word
import GHC.Generics

import "this" Clickable.Protocol.Value qualified as Value

data HaskellMessage
  = EvalExpr Expr
  -- ^ Evaluate expression, expect the result to be returned by
  -- 'Return' message
  | HotReload
  -- ^ Used under dev server, won't return anything
  | Halt
  -- ^ Signal that current process completed, won't return anything
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data JavaScriptMessage
  = Start StartFlags
  | Return Value.Value
  | TriggerCallbackMsg Value.Value CallbackId
  | BeforeUnload
  -- ^ Fired from addEventListener("beforeunload") listener. Won't
  -- work under the DevServer!
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data StartFlags = StartFlags
  { initial_url :: Location
  , window_inner_size :: (Int64, Int64)
  , devserver_connection_id :: Maybe Int64
  } deriving stock (Generic, Show)
    deriving anyclass (Binary)

data Location = Location
  { protocol :: Text
  -- ^ A string containing the protocol scheme of the URL, including
  -- the final ':'
  , hostname :: Text
  -- ^ A string containing the domain of the URL.
  , port :: Text
  -- ^ A string containing the port number of the URL.
  , pathname :: Text
  -- ^ A string containing an initial '/' followed by the path of the
  -- URL, not including the query string or fragment.
  , search :: Text
  -- ^ A string containing a '?' followed by the parameters or
  -- "querystring" of the URL
  , hash :: Text
  -- ^ A string containing a '#' followed by the fragment identifier
  -- of the URL.
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (Binary, Value.FromJSValue, Value.ToJSValue)

-- | Strict Lambda calculus with arbitrary side-effects, meant to be
-- used as commands executed in the JavaScript side, optimized for
-- non-blocking execution and minimizing round-trips.
data Expr
  = Null
  -- ^ Represents null or undefined values
  | Boolean Bool
  -- ^ JavaScript boolean value
  | I64 Int64
  -- ^ JavaScript integer number
  | F64 Double
  -- ^ JavaScript floating point number
  | String Text
  -- ^ JavaScript string
  | Array [Expr]
  -- ^ JavaScript array
  | Object [(Text, Expr)]
  -- ^ JavaScript object
  | Uint8Array ByteString
  -- ^ Raw byte array

  | Dot Expr Text
  -- ^ Read string property of an object. @(Dot (Id "document")
  -- "body")@ is equivalent to @document.body@ JavaScript expression
  | SetProp Expr Text Expr
  -- ^ Assign a value to a string property of an object @(AssignProp
  -- (Id "foo") "bar" (Str "baz"))@ is equivalent to @foo['bar'] =
  -- baz;@ JavaScript expression. Evaluates into its right-hand side
  -- expression.
  | Ix Expr Int64
  -- ^ Read value from an integer index of an object. @(Ix (Id
  -- "foo") 0)@ is equivalent to @foo[0]@ JavaScript expression

  | Add Expr Expr
  -- ^ Binary addition @(Add 256 5647)@ is equivalent to @256 + 5647@
  | Subtract Expr Expr
  -- ^ Binary substraction @(Subtract 256 5647)@ is equivalent to @256 - 5647@
  | Multiply Expr Expr
  -- ^ Binary multiplication @(Multiply 256 5647)@ is equivalent to @256 * 5647@
  | Divide Expr Expr
  -- ^ Binary division @(Divide 256 5647)@ is equivalent to @256 / 5647@

  | Id Text -- ^ Lookup an identifier in current lexical scope
  | Lam Expr
  -- ^ Introduce a lambda function. Arguments can be accessed via 'Arg
  -- 0 0'
  | Arg Word8 Word8
  -- ^ Lookup an argument in the current argument scope. Separate
  -- scope for argument from regular lexical scope required for
  -- performance reasons. First field is "De Bruijn index" pointing to
  -- nth-lambda counting outward from current expression. Second
  -- number is positional argument for that lambda (each lambda
  -- receives multiple arguments in a vector, see
  -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/arguments)
  -- De Bruijn indicies are notoriously hard to write manually, it's
  -- one of the reasons this protocol is much less convenient than FFI
  -- with JavaScript. One solution I found is that for complex code I
  -- write regular JavaScript and run it with 'Eval'.
  | Apply Expr [Expr]
  -- ^ Apply a function to arbitrary length arguments. @Apply (Id
  -- "encodeURIComponent") [Str "#"]@ going to evaluate to @String "%23"@
  | Call Expr Text [Expr]
  -- ^ Call a method of an object @Call (Id "console") "log" [Str
  -- "Hi!"]@ is equivalent to @console.log('Hi!')@ JavaScript code

  | AssignVar VarId Expr
  -- ^ Assign a value to VarId allocated in haskell side. This way
  -- haskell can save certain values between WASM reactor invocations
  | FreeVar VarId
  -- ^ Free a variable allocated with @AssignVar@
  | Var VarId
  -- ^ Retrieve the value of the variable
  | FreeScope ReactiveScope
  -- ^ Free all the resources assosiated with the given ReactiveScope

  | HtmlBuilder Expr Expr
  | InsertNode Expr Expr
  | CreateElement Text
  | CreateElementNS Text Text
  | CreateTextNode Text
  | ElementProp Expr Text Expr
  | ElementAttr Expr Text Text
  | InsertClassList Expr [Text]
  | RemoveClassList Expr [Text]
  | UpdateTextNode Expr Text
  | InsertBoundary Expr
  | ClearBoundary Expr Bool

  | AddEventListener ReactiveScope Expr Expr Expr
  -- ^ @AddEventListener rscope target eventName listener@ is
  -- equivalent to @target.addEventListener(eventName, listener)@ it
  -- returns @FinalizerId@ integer identifier that can be used in
  -- 'RemoveEventListener', but calling 'RemoveEventListener' is not
  -- required, it'll be called authomatically when given ReactiveScope
  -- will be freed with 'FreeScope'
  | SetTimeout ReactiveScope Expr Int64
  -- ^ Returns FinalizerId
  | ApplyFinalizer ReactiveScope Expr
  -- ^ Actuate given finalizer before the ReactiveScope is freed

  | RevSeq [Expr]
  -- ^ Sequence of the expressions in reverse order. It will be
  -- evaluated from the end to the beggining of the list. Returns
  -- whatever the last expression (from the head of the list)
  -- evaluates into. Order is reversed to support fast insertion of
  -- new instructions
  | Eval Text
  -- ^ Evaluate arbitrary JavaScript code @(Eval "setTimeout(() =>
  -- console.log('Hi!'), 1000)")@ will print a message with one second
  -- delay
  | TriggerCallback CallbackId Expr
  -- ^ As a side-effect emits `TriggerCallbackMsg` message to Haskell
  | UncaughtException Text
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

valueToExpr :: Value.Value -> Expr
valueToExpr = \case
  Value.Null -> Null
  Value.Bool a -> Boolean a
  Value.I64 a -> I64 a
  Value.F64 a -> F64 a
  Value.String a -> String a
  Value.Array xs -> Array $ fmap valueToExpr xs
  Value.Object kv -> Object $ fmap (\(k, v) -> (k, valueToExpr v)) kv
  Value.Uint8Array a -> Uint8Array a

data VarId = VarId ReactiveScope Int64
  deriving stock (Generic, Show, Ord, Eq)
  deriving anyclass (Binary)

newtype CallbackId = CallbackId { unCallbackId :: Int64 }
  deriving newtype (Show, Num, Binary, Ord, Eq)

newtype ReactiveScope = ReactiveScope { unReactiveScope :: Int64 }
  deriving newtype (Show, Num, Binary, Ord, Eq)

newtype FinalizerId = FinalizerId { unFinalizerId :: Int64 }
  deriving newtype (Show, Num, Binary, Ord, Eq)
