{-# LANGUAGE DerivingVia #-}
module Clickable.Protocol where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.String
import Data.Word
import GHC.Generics

import Clickable.Protocol.Value

data HaskellMessage
  = EvalExpr Int32Le Expr
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
  | Return Int32Le Value
  | TriggerEventMsg Value EventId
  | BeforeUnload
  -- ^ Fired from addEventListener("beforeunload") listener. Won't
  -- work under the DevServer!
  deriving (Generic, Show)
  deriving anyclass (Binary)

newtype StartFlags = StartFlags {unStartFlags :: Value}
  deriving newtype (Generic, Show, Binary)

-- | Strict Lambda calculus with arbitrary side-effects, meant to be
-- used as commands executed in the JavaScript side, optimized for
-- non-blocking execution and minimizing round-trips.
data Expr
  = Null
  -- ^ null or undefined values
  | Bool Bool
  -- ^ JavaScript boolean value
  | I8 Int8
  | I16 Int16Le
  | I32 Int32Le
  | I64 Int64Le

  | U8 Word8
  | U16 Word16Le
  | U32 Word32Le
  | U64 Word64Le

  | F32 Float32Le
  | F64 Float64Le

  | Str Text -- ^ JavaScript string
  | Arr [Expr] -- ^ JavaScript array
  | Obj [(Text, Expr)] -- ^ JavaScript object
  | U8Arr ByteString -- ^ Raw byte array

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
  -- ^ Free variable allocated with @AssignVar@
  | Var VarId
  -- ^ Retrieve the value of the variable
  | FreeScope ResourceScope
  -- ^ Free all the resources assosiated with the given ResourceScope

  | AskDomBuilder
  | SupplyDomBuilder DomBuilder Expr

  | InsertNode Expr
  | ElementProp Text Expr
  | ElementAttr Text Text
  | ClassListAdd [Text]
  | ClassListRemove [Text]
  | InsertBrackets
  | ClearBrackets Bool

  | CreateElement Text
  | CreateElementNS Text Text
  | CreateTextNode Text
  | UpdateTextNode Expr Text

  | AddEventListener ResourceScope Expr Expr Expr
  -- ^ @AddEventListener rscope target eventName listener@ is
  -- equivalent to @target.addEventListener(eventName, listener)@ it
  -- returns @FinalizerId@ integer identifier that can be used in
  -- 'RemoveEventListener', but calling 'RemoveEventListener' is not
  -- required, it'll be called authomatically when given ResourceScope
  -- will be freed with 'FreeScope'
  | ConnectResource ResourceScope Expr
  -- ^ Returns FinalizerId
  | SetTimeout ResourceScope Expr Int32Le
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
  | TriggerEvent EventId Expr
  -- ^ Emits `TriggerEventMsg` as a side-effect
  | UncaughtException Text
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

valueToExpr :: Value -> Expr
valueToExpr = \case
  Vnull -> Null
  Vbool a -> Bool a

  Vi8 a -> I8 a
  Vi16 a -> I16 a
  Vi32 a -> I32 a
  Vi64 a -> I64 a

  Vu8 a -> U8 a
  Vu16 a -> U16 a
  Vu32 a -> U32 a
  Vu64 a -> U64 a

  Vf64 a -> F64 a
  Vf32 a -> F32 a

  Vstr a -> Str a
  Varr xs -> Arr $ fmap valueToExpr xs
  Vobj kv -> Obj $ fmap (\(k, v) -> (k, valueToExpr v)) kv
  Vu8arr a -> U8Arr a

toExpr :: ToValue a => a -> Expr
toExpr = valueToExpr . toValue

newtype DomBuilder = DomBuilder {unDomBuilder :: Expr}
  deriving newtype (Show, Binary)

data VarId = VarId ResourceScope Int32Le
  deriving stock (Generic, Show, Ord, Eq)
  deriving anyclass (Binary)

newtype FinalizerId = FinalizerId {unFinalizerId :: Int32}
  deriving newtype (Show, Ord, Eq)
  deriving Binary via Int32Le

newtype ResourceScope = ResourceScope {unResourceScope :: Int32}
  deriving newtype (Show, Ord, Eq)
  deriving Binary via Int32Le

newtype EventId = EventId {unEventId :: Int32}
  deriving newtype (Show, Ord, Eq)
  deriving Binary via Int32Le

newtype UnsafeJavaScript = UnsafeJavaScript {unUnsafeJavaScript :: Text}
  deriving newtype (IsString, Show, Semigroup, Monoid, Binary)
