module APL.Eval
  ( Val (..),
   eval,
  )
where

import APL.AST (Exp (..))

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)


type Error = String

{- eval :: Exp -> Val
eval (CstInt x) = ValInt x
eval (Add (CstInt x) (CstInt y)) = ValInt (x + y)
eval (Sub (CstInt x) (CstInt y)) = ValInt (x - y)
eval (Mul (CstInt x) (CstInt y)) = ValInt (x * y)
eval (Div (CstInt x) (CstInt y)) = ValInt (x `div` y)
eval (Pow (CstInt x) (CstInt y)) = ValInt (x ^ y) -}

eval :: Exp -> Either Error Val
eval (CstInt x) = Right $ ValInt x
eval (Add e1 e2) =
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x + y
    (Right _, Right _) -> Left "Non-integer operand"
eval (Sub e1 e2) = 
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x - y
    (Right _, Right _) -> Left "Non-integer operand"
eval (Mul e1 e2) = 
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x * y
    (Right _, Right _) -> Left "Non-integer operand"

eval (Div e1 e2) = 
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x `div` y
    (Right _, Right _) -> Left "Non-integer operand"

--eval (Sub (CstInt x) (CstInt y)) = Right $ ValInt (x - y)
--eval (Mul (CstInt x) (CstInt y)) = Right $ ValInt (x * y)