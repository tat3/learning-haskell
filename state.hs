import Control.Monad.State
import Control.Monad.State

bind a b = state $ \s ->
    let (r1, s1) = runState a s
        (r2, s2) = runState (b r1) s1
    in (r2, s2)
return' x = state $ \s -> (x, s)
get'      = state $ \s -> (s, s)
put'    x = state $ \_ -> ((), x)

fib x = (`evalState` (0, 1)) $
    (replicateM_ (x - 1) $
        get' `bind` \(a, b) -> put' (b, a + b)) `bind` \_ ->
    get' `bind` \v -> return' $ snd v


fib' x = fst $ (`runState` (0, 1)) $ do
    replicateM_ (x - 1) $ do
        (a, b) <- get 
        put (b, a + b)
    v <- get
    return $ snd v
 
main = do
    print $ fib  10
    print $ fib' 10
