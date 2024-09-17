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
askEnv = EvalM $ \env -> env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM g)=
  EvalM $ \env -> g (f env)

instance Functor EvalM where
  fmap :: (a -> b) -> EvalM a -> EvalM b
  fmap _ (EvalM (Left e)) = EvalM $ Left e
  fmap f (EvalM (Right x)) = EvalM $ Right (f x)
  -- Alternatively: fmap = liftM

instance Applicative EvalM where
  pure :: a -> EvalM a
  pure x = EvalM $ Right x
  (<*>) :: EvalM (a -> b) -> EvalM a -> EvalM b
  EvalM (Left e)  <*> _               = EvalM (Left e)
  _               <*> EvalM (Left e)  = EvalM (Left e)
  EvalM (Right f) <*> EvalM (Right x) = EvalM $ Right (f x)
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
    EvalM x >>= f 
      = case x of
        Left err -> EvalM $ Left err
        Right x' -> f x'


failure :: String -> EvalM a
failure str = EvalM $ Left str

runEval :: EvalM a -> Either Error a
runEval (EvalM envEmpty x) =  x

---expresssion er still exp
---val will be wrapped as evalM val
--funktion eval skal omskrives

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) ->Env -> Exp -> Exp -> EvalM Val
evalIntBinOp f env e1 e2 = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) ->  Env -> Exp -> Exp -> EvalM Val
evalIntBinOp' f env e1 e2 = 
  evalIntBinOp f' env e1 e2
  where
    f' x y = pure $ f x y

eval ::  Exp -> EvalM Val
eval _ (CstInt x) = pure $ ValInt x
eval _ (CstBool b) = pure $ ValBool b
eval env (Var v) = do
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v

eval env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2

eval env (Div e1 e2) = evalIntBinOp checkedDiv env e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y

eval env (Pow e1 e2) = evalIntBinOp checkedPow env e1 e2
  where
    checkedPow x y = 
      case (y < 0) of
        True -> failure "Negative power"
        _ -> pure $ x ^ y

eval env (Eql e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (ValInt x,  ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) ->pure $ ValBool $ x == y
    (_,_) -> failure "Invalid operands to equality"


-- when eval returns a value of type EvalM Val, 
---which is a wrapper around Either Error Val. You cannot pattern 
---match on the value inside EvalM directly like that.

--Solution:
--You need to 
--1. extract the result from EvalM using the do notation 
--2. or pattern matching on Either. Here's the corrected version of the If clause:

eval env (If cond e1 e2) = do
  b <- eval env cond
  case b of
    --EvalM (Left err) -> failure err
    ValBool True -> eval env e1
    ValBool False -> eval env e2
    _ -> failure "Non-boolean conditional."

eval env (Let var e1 e2) = do
    v1 <- eval env e1 
    eval (envExtend var v1 env) e2

eval env (Lambda var body) = do
  pure $ ValFun env var body

eval env (Apply e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (ValFun envFun paraName body, argVal ) -> 
        let newEnv = envExtend paraName argVal envFun
          in eval newEnv body
    (_, _) -> failure "Cannot apply non-function"


--eval env e1 returns an EvalM Val, you cannot directly pattern 
---match on it.
{- 
eval env (TryCatch e1 e2) = do
  result <- eval env e1
  case result of
    Left _ -> eval env e2
    Right x -> pure x
 -}

{- eval env (TryCatch e1 e2) = 
  case eval env e1 of
    Left _ -> eval env e2
    Right x -> pure x -}

eval env (TryCatch e1 e2) = 
  eval env e1 `catch` eval env e2    

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) =  EvalM $
  case m1 of
    Left _ -> m2 
    Right x -> Right x