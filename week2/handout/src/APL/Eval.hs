{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import Text.Printf (vFmt)
import Foreign.C (errnoToIOError)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

--newtype similar to data with one constructor which has a value
--Change the definition of EvalM a such that it can represent two cases:
-- either a value of type a, or an error of type Error.
newtype EvalM a = EvalM (Env -> Either Error a )
---Either Error a  means Left is Error, Right is a 
--EvalM a is a wrapper around Either Error a.

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM g)=
  EvalM $ \env -> g (f env)

instance Functor EvalM where
  fmap :: (a -> b) -> EvalM a -> EvalM b
  fmap f (EvalM x) =
    EvalM $ \env -> case x env of 
      Right v -> Right $ f v
      Left err -> Left err

  --fmap _ (EvalM (Left e)) = EvalM $ Left e
  --fmap f (EvalM (Right x)) = EvalM $ Right (f x)
  -- Alternatively: fmap = liftM

instance Applicative EvalM where
  pure :: a -> EvalM a
  pure x = EvalM $ \_env -> Right x
  
  (<*>) :: EvalM (a -> b) -> EvalM a -> EvalM b
  EvalM ef <*> EvalM ex = EvalM $ \env ->
    case (ef env, ex env) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right f, Right x) -> Right (f x)

--ef is function env -> Either Error a
--so ef env return Either Error a 
-- Alternatively: (<*>) = ap


{- 
instance Monad EvalM where  
    (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
    EvalM x >>= f 
      = EvalM $ 
      case x of
        Left err -> Left err
        Right x' ->
          let EvalM y = f x'
           in y -}

instance Monad EvalM where  
    (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
    EvalM x >>= f  = EvalM $ \env ->
      case x env of
        Left err ->  Left err
        Right x' -> 
          let EvalM y = f x'
           in y env


failure :: String -> EvalM a
failure str = EvalM $ \_env -> Left str

runEval :: EvalM a -> Either Error a
runEval (EvalM m) =  m envEmpty

---expresssion er still exp
---val will be wrapped as evalM val
--funktion eval skal omskrives


catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) =  EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval  e1
  v2 <- eval  e2
  case (v1, v2) of
    (ValInt x, ValInt y) ->  ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 = 
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval ::  Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v

eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-)  e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*)  e1 e2

eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y

eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y = 
      case (y < 0) of
        True -> failure "Negative power"
        _ -> pure $ x ^ y

eval (Eql e1 e2) = do
  v1 <- eval  e1
  v2 <- eval  e2
  case (v1, v2) of
    (ValInt x,  ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) ->pure $ ValBool $ x == y
    (_,_) -> failure "Invalid operands to equality"

eval (If cond e1 e2) = do
  b <- eval cond
  case b of
    --EvalM (Left err) -> failure err
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."

eval (Let var e1 e2) = do
    v1 <- eval e1 
    localEnv (envExtend var v1) $ eval e2

eval (Lambda var body) = do
  env <-askEnv
  pure $ ValFun env var body

eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun envFun paraName body, argVal ) -> 
        localEnv(const $ envExtend paraName argVal envFun) $
           eval body
    (_, _) -> failure "Cannot apply non-function"

eval (TryCatch e1 e2) = 
  eval e1 `catch` eval e2    

