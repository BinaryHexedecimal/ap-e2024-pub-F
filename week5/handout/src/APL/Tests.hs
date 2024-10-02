module APL.Tests where

import APL.AST (Exp (..), VName)
import Test.QuickCheck (Gen
                        ,elements
                        ,listOf
                        ,oneof
                        ,Arbitrary(arbitrary, shrink)
                        ,sized
                       -- ,shrink
                        )
import Data.Bool (Bool)
import APL.Eval( Val (..),
    Env,
    eval,
    runEval
  )
import GHC.Base (VecElem(Int16ElemRep))
instance Arbitrary Exp where
    arbitrary = sized genExp

    shrink (Add e1 e2) =
        e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]

    shrink (Sub e1 e2) =
        e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]

    shrink (Mul e1 e2) =
        e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]

    shrink (Div e1 e2) =
        e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]

    shrink (Pow e1 e2) =
        e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
    shrink (Eql e1 e2) =
        e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]

    shrink (If e1 e2 e3) =
        e1 : e2 : e3: [If e1' e2 e3| e1' <- shrink e1] ++ [If e1 e2' e3 | e2' <- shrink e2] ++ [If e1 e2 e3'| e3' <- shrink e3]

    shrink (Var x) =
        [Var x' | x' <- shrink x, not (null x')]

        
    shrink (Lambda x e) =
        e :[Lambda x' e| x' <- shrink x, not (null x')]++ [Lambda x e' | e' <- shrink e]

    shrink (Apply e1 e2) =
        e1 : e2 : [Apply e1' e2|e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]

    shrink (TryCatch e1 e2) =
        e1 : e2 : [TryCatch e1' e2|e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]

    shrink (Let x e2 e3) =
        e2 : [Let x' e2 e3| x' <- shrink x, not(null x')] ++ [Let x e2' e3 | e2' <- shrink e2] ++ [Let x e2 e3'| e3' <- shrink e3]




genVar :: Gen VName
genVar = do
    alpha <- elements ['a'..'z']
    alphaNums <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
    pure (alpha : alphaNums)

genExp :: Int -> Gen Exp
genExp size = 
    if size <= 1
    then Var <$> genVar
    else
        let half =(size - 1 ) `div` 2
            third = (size - 1 ) `div` 3
            in oneof [Var <$> genVar
                    ,Lambda <$> genVar <*> genExp (size - 1)
                    ,Apply <$> genExp half <*> genExp half
                    ,Add <$> genExp half <*> genExp half
                    ,Sub <$> genExp half <*> genExp half
                    ,Mul <$> genExp half <*> genExp half
                    ,Div <$> genExp half <*> genExp half
                    ,Pow <$> genExp half <*> genExp half
                    ,Eql <$> genExp half <*> genExp half
                    ,If <$> genExp third <*> genExp third <*> genExp third
                    ,TryCatch <$> genExp half <*> genExp half
                    ,Let <$> genVar <*> genExp half <*> genExp half
                    ,CstInt <$> arbitrary 
                    ,CstBool <$> arbitrary 
           ]

-- data Exp
--   = CstInt Integer
--   | CstBool Bool


prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc a b c = 
    (a + b) + c  == a + (b + c)

-- prop_aplAddAssoc :: Integer -> Integer -> Integer -> Bool
-- prop_aplAddAssoc e1 e2 e3 =
--     runEval(eval (Apply (Apply e1 e2) e3)==runEval(eval (Apply e1 (Apply (e2 e3)))))

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval (Add e1 (Add e2 e3)))

