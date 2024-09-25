{- fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
 -}
import Control.Monad (ap)
import Control.Applicative ((<*>))

data Free e a
 = Pure a
 | Free (e (Free e a)) 

instance (Functor e ) => Functor (Free e) where
    fmap f (Pure x) = Pure $ f x
    fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e ) => Applicative (Free e) where
    pure = Pure
    (<*>) = ap
    
instance (Functor e ) => Monad (Free e) where
    Pure x >>= f = f x
    Free g >>= f = Free $ h <$> g 
        where h x = x >>= f  

----a Fibop effect
data FibOp a 
    = FibLog String a
    | FibMemo Int (FibM Int) (Int -> a)
--- Int n , show this effect refers to result of fib(n)
---FibM Int , computation fib(n)
---continuation Int -> a

instance Functor FibOp where
    fmap f (FibLog s x) = FibLog s $ f x
    fmap f (FibMemo n m c) = FibMemo n m $ \y -> f (c y)

fibMemo :: Int -> FibM Int -> FibM Int
fibMemo n m =  Free $ FibMemo n m pure

----fib Monad
type FibM a = Free FibOp a 

---accessor function
fibLog :: String -> FibM()
fibLog s = Free $ FibLog s $ pure()

--fib
fib :: Int -> FibM Int
fib 0 = pure 1
fib 1 = pure 1
fib n =  fibMemo n $ do
    fibLog $ "fib(" ++ show n ++ ")"
    x <- fib(n-1)
    y <- fib(n-2)
    pure $ x + y


ioFibM :: FibM a -> IO a
ioFibM (Pure x ) = pure x
ioFibM (Free (FibLog s x)) = do
    putStrLn s
    ioFibM x    



pureFibM :: FibM a -> a
pureFibM (Pure x) = x
pureFibM (Free (FibLog _ c)) = pureFibM c
pureFibM (Free (FibMemo _ x c)) = pureFibM $ c $ pureFibM x




logFibM :: FibM a -> (a, [String])
logFibM (Pure x ) = (x, [])
logFibM (Free (FibLog s c) ) = 
    let (x' , msgs) = logFibM c
    in (x', msgs ++ [s])


memoFibM :: FibM a -> a
memoFibM m = fst $ memo [] m
    where
        memo :: [(Int, Int)] -> FibM a -> (a, [(Int, Int)])
        memo cache (Pure x) =(x, cache)
        memo cache (Free (FibMemo n fn c)) =
            case lookup n cache of
                Just res -> memo cache $ c res
                Nothing -> 
                    let (fn', cache') = memo cache fn
                        in memo((n, fn'):cache')(c fn')
        memo cache (Free (FibLog _ x)) = memo cache x

main :: IO()
main = do
    --let a = fib 5
    --ioFibM $ fib 5
    
    --let a = logFibM $ fib 5
    --in putStrLn a


    -- let (result, logs) = logFibM $ fib 5  -- Unpack the result and log messages
    -- mapM_ putStrLn logs                   -- Print each log message
    -- putStrLn $ "Result: " ++ show result  -- Print the result
    let a = memoFibM $ fib 5
    putStrLn $ show a
    putStrLn $ "Hello world"
