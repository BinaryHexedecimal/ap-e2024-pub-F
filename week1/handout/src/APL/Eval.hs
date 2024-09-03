module APL.Eval
  ( Val (..),
   eval,
   Env,
   envEmpty
  )
where

import APL.AST (Exp (..), VName)


data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String
type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []
envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v,val):env
envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env


--help function, only +-*
unwrap :: Env -> (Integer -> Integer -> Integer) -> Exp -> Exp -> Either Error Val
unwrap env f e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt (f x y)
    (Right _, Right _) -> Left "Non-integer operand"


eval :: Env -> Exp -> Either Error Val
eval env (CstInt x) = Right $ ValInt x
eval env (CstBool x) = Right $ ValBool x
eval env (Add e1 e2) = unwrap env (+) e1 e2
eval env (Sub e1 e2) = unwrap env (-) e1 e2
eval env (Mul e1 e2) = unwrap env (*) e1 e2
eval env (Div e1 e2) = 
    case (eval env e1, eval env e2) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> checkedDiv x y
      (Right _, Right _) -> Left "Non-integer operand"
    where  
      checkedDiv _ 0 = Left "Division by zero"
      checkedDiv x y = Right $ ValInt (x `div` y)

eval env (Pow e1 e2) = 
    case (eval env e1, eval env e2) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right (ValInt x), Right (ValInt y)) -> checkedPow x y
      (Right _, Right _) -> Left "Non-integer operand"
    where  
      checkedPow x y  = 
        if y <0 
          then Left "exp negative"
          else Right $ ValInt (x^y)


eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
  (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
  (Right _, Right _) -> Left "Mixing types is not allowed to compared"


eval env (If cond e1 e2) =
    case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "The first expression is not boolean"

eval env (Var v) =
  case envLookup v env of
    Just x -> Right x
    Nothing -> Left $ "unknown variable" ++ v

eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2
