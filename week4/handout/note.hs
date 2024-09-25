data Free e a = Pure a
            | Free (e (Free e a))
--- e effects, (Free e a) like (IO a)
--- e is a type constructor
--- first do e, then execute (Free e a)
---doing effect, meaning interpretation function(defined for monad)
---one monad, can multiple different interpretation function.



---implement Reader in terms of Free
---ask r, produce a
data ReadOp r a = ReadOp (r -> a)
--- r -> a  value is called continuation

instance Functor (ReadOp r) where
    fmap f (ReadOp g) = ReadOp $ \x -> f (g x)


---ReadOp r is a kind of e
type Reader r a = Free (ReadOp r) a
--    = Free e a = Pure a| Free(e (Free e a))

--interpretation function, run monad and give meaning to effect (not do effect)
runReader :: r -> Reader r a -> a
runReader _ (Pure x) = x   ----x is a 
runReader r (Free (ReadOp g)) = runReader r (g r)
----g is e a
----e is (ReadOp g), (Free e a) is argument 


ask :: Reader r r
ask = Free $ ReadOp $ \x -> Pure x

--- e is (ReadOp )
instance (Functor e ) => Functor (Free e ) where
