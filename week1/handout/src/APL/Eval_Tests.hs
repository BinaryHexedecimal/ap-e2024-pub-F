
module APL.Eval_Tests (tests) where


import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (TestTree, testGroup)


tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [testCase "Constant" $
        eval envEmpty (CstInt 2)
          @?= Right (ValInt 2) ,
    ---
    testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 3))
          @?= Right (ValInt 5) ,
    ---
    testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",

    ---
    testCase "Sub" $
        eval envEmpty (Sub (CstInt 9) (CstInt 3))
          @?= Right (ValInt 6),

    ---
    testCase "Mul" $
        eval envEmpty (Mul (CstInt 9) (CstInt 3))
          @?= Right (ValInt 27),

    ---
    testCase "Div" $
        eval envEmpty (Div (CstInt 9) (CstInt 3))
          @?= Right (ValInt 3),
    ---
    testCase "Eql (false)" $
        eval envEmpty (Eql (CstBool True) (CstBool False))
          @?= Right (ValBool False),
    --
    testCase "Eql (true)" $
      eval envEmpty (Eql (CstInt 2) (CstInt 2))
        @?= Right (ValBool True),
    --
    testCase "If" $
      eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 1)))
        @?= Right (ValInt 2)
    ]
