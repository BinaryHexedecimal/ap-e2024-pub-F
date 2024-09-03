
module APL.Eval_Tests (tests) where


import APL.AST (Exp (..))
import APL.Eval (Val (..), eval)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (TestTree, testGroup)

--import APL.AST ()
--import APL.Eval ()
--import Test.Tasty.HUnit ()

{- data Val
  = ValInt Integer
  deriving (Eq, Show)

eval :: Exp -> Val
eval (CstInt x) = ValInt x -}

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [testCase "Constant" $
        eval (CstInt 2)
          @?= Right (ValInt 2) ,
    ---
    testCase "Add" $
        eval (Add (CstInt 2) (CstInt 3))
          @?= Right (ValInt 5) ,
    ---
    testCase "Add (wrong type)" $
        eval (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",

    ---
    testCase "Sub" $
        eval (Sub (CstInt 9) (CstInt 3))
          @?= Right (ValInt 6),

    ---
    testCase "Mul" $
        eval (Mul (CstInt 9) (CstInt 3))
          @?= Right (ValInt 27),

    ---
    testCase "Div" $
        eval (Div (CstInt 9) (CstInt 3))
          @?= Right (ValInt 3)
    -- ---
    -- testCase "Div0" $
    --     eval (Div (CstInt 9) (CstInt 0))
    --       @?= ValInt 3,      

    --           ---
    -- testCase "Pow" $
    --     eval (Pow (CstInt 2) (CstInt 3))
    --       @?= ValInt 8,

    -- testCase "Pow Negative Exponent" $
    --     eval (Pow (CstInt 2) (CstInt (-3)))
    --       @?= ValInt 0
    ]

{- eval (Add (CstInt x) (CstInt y)) = ValInt (x + y)
eval (Sub (CstInt x) (CstInt y)) = ValInt (x - y)
eval (Mul (CstInt x) (CstInt y)) = ValInt (x * y)
eval (Div (CstInt x) (CstInt y)) = ValInt (x `div` y)
eval (Pow (CstInt x) (CstInt y)) = ValInt (x ^ y) -}