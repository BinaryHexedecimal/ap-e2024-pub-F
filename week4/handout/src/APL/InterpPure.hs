module APL.InterpPure (runEval) where

import APL.Monad

---interpretation
runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)

      -----r is Env
      ---k is Env -> a
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
     ----similar

    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    
    
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m

    runEval' r s (Free (PrintOp p m)) = 
      let (ps, res) = runEval' r s m
        in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)