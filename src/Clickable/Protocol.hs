module Clickable.Protocol where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.String
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
  | TriggerCallbackMsg Value.Value SourceId
  | BeforeUnload
  -- ^ Fired from addEventListener("beforeunload") listener. Won't
  -- work under the DevServer!
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

newtype StartFlags = StartFlags {unStartFlags :: Value.Value}
  deriving newtype (Generic, Show, Binary)

-- | Strict Lambda calculus with arbitrary side-effects, meant to be
-- used as commands executed in the JavaScript side, optimized for
-- non-blocking execution and minimizing round-trips.
data Expr
  = Null
  -- ^ Represents null or undefined values
  | Boolean Bool
  -- ^ JavaScript boolean value
  | I32 Value.Int32Le
  -- ^ JavaScript integer number
  | F64 Value.Float64
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

  | Plus Expr Expr
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
  | FreeScope ResourceScope
  -- ^ Free all the resources assosiated with the given ResourceScope

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

  | AddEventListener ResourceScope Expr Expr Expr
  -- ^ @AddEventListener rscope target eventName listener@ is
  -- equivalent to @target.addEventListener(eventName, listener)@ it
  -- returns @FinalizerId@ integer identifier that can be used in
  -- 'RemoveEventListener', but calling 'RemoveEventListener' is not
  -- required, it'll be called authomatically when given ResourceScope
  -- will be freed with 'FreeScope'
  | ConnectResource ResourceScope Expr
  -- ^ Returns FinalizerId
  | SetTimeout ResourceScope Expr Int64
  -- ^ Returns FinalizerId
  | ApplyFinalizer ResourceScope Expr
  -- ^ Actuate given finalizer before the ResourceScope is freed

  | RevSeq [Expr]
  -- ^ Sequence of the expressions in reverse order. It will be
  -- evaluated from the end to the beggining of the list. Returns
  -- whatever the last expression (from the head of the list)
  -- evaluates into. Order is reversed to support fast insertion of
  -- new instructions
  | Eval UnsafeJavaScript
  -- ^ Evaluate arbitrary JavaScript code @(Eval "setTimeout(() =>
  -- console.log('Hi!'), 1000)")@ will print a message with one second
  -- delay
  | TriggerCallback SourceId Expr
  -- ^ As a side-effect emits `TriggerCallbackMsg` message to Haskell
  | UncaughtException Text
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

valueToExpr :: Value.Value -> Expr
valueToExpr = \case
  Value.Null -> Null
  Value.Bool a -> Boolean a
  Value.I32 a -> I32 a
  Value.F64 a -> F64 a
  Value.String a -> String a
  Value.Array xs -> Array $ fmap valueToExpr xs
  Value.Object kv -> Object $ fmap (\(k, v) -> (k, valueToExpr v)) kv
  Value.Uint8Array a -> Uint8Array a

toExpr :: Value.ToValue a => a -> Expr
toExpr = valueToExpr . Value.toValue

data VarId = VarId ResourceScope Value.Int32Le
  deriving stock (Generic, Show, Ord, Eq)
  deriving anyclass (Binary)

newtype FinalizerId = FinalizerId { unFinalizerId :: Value.Int32Le }
  deriving newtype (Show, Num, Ord, Eq, Binary)

newtype ResourceScope = ResourceScope {unResourceScope :: Value.Int32Le}
  deriving newtype (Show, Num, Ord, Eq, Binary)

newtype SourceId = SourceId {unSourceId :: Value.Int32Le}
  deriving newtype (Show, Num, Ord, Eq, Binary)

newtype UnsafeJavaScript = UnsafeJavaScript {unUnsafeJavaScript :: Text}
  deriving newtype (IsString, Show, Semigroup, Monoid, Binary)
