module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    stateInitial,
    askEnv,
    modifyEffects,
    localEnv,
    getState,
    putState,
    modifyState,
    evalKvGet,
    evalKvPut,
    evalPrint,
    failure,
    catch,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g
-----g = e (Free e a)

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where h x = x >>= f

---effect
data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)  ---retrieve state
  | StatePutOp State a   ----replace with a new
  | PrintOp String a
  | ErrorOp Error

instance Functor EvalOp where
    --fmap f (ReadOp k) = ReadOp $ \x -> f (k x)
  ---k is Env -> a
  -- x is a Env here
  fmap f (ReadOp k) = ReadOp $ f . k
----
  ---fmap f (StateGetOp k) = StateGetOp $ \x -> f (k x)
  fmap f (StateGetOp k) = StateGetOp $ f . k

  fmap f (StatePutOp s m) = StatePutOp s $ f m

  fmap f (PrintOp s m) = PrintOp s $ f m

  fmap _ (ErrorOp e) = ErrorOp e  

type EvalM a = Free EvalOp a

---interface function
askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

getState :: EvalM State
getState = Free $ StateGetOp $ \state -> pure state

putState :: State -> EvalM ()
putState s = Free $ StatePutOp s $ pure()

---修改状态的函数，通过获取当前状态
---并应用传入的函数 f 来更新状态。
modifyState :: (State -> State) -> EvalM ()
modifyState f = do
  s <- getState
  putState $ f s

---make local environment
-----localEnv: 该函数允许在本地环境中修改 Env，
----在执行 EvalM a 时暂时修改环境，而不改变全局环境。
---它使用 modifyEffects 来修改环境的效果。
localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g op = op

---modifyEffects: 这是一个通用的函数，
---用于修改 Free monad 中的效果。它接收一个函数 g，
----这个函数会修改 e (Free e a) 类型的效果，
---并将其转换为 h (Free e a)。
---该函数递归地应用修改到整个 Free monad 结构。
modifyEffects :: (Functor e, Functor h) => (e (Free e a) -> h (Free e a)) -> Free e a -> Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free ea) = Free $ modifyEffects g <$> g ea


evalPrint :: String -> EvalM ()
evalPrint p = Free $ PrintOp p $ pure()


failure :: String -> EvalM a
failure s = Free $ ErrorOp s  

catch :: EvalM a -> EvalM a -> EvalM a
catch = error "To be completed in assignment 4."

evalKvGet :: Val -> EvalM Val
evalKvGet = error "To be completed in assignment 4."

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut = error "To be completed in assignment 4."
