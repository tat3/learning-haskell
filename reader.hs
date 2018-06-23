import Control.Monad.Reader

bind a b = reader $ \r ->
    runReader (b (runReader a r)) r
return' x = reader $ \_ -> x
ask' = reader $ \r -> r
local' f m = reader $ \r -> runReader m $ f r


test x = (`runReader` x) $
    ask' `bind` \a ->
    (local' (+ 1) $
        ask' `bind` \b' ->
        return' b') `bind` \b ->
    ask' `bind` \c ->
    return' (a, b, c)

test' x = (`runReader` x) $ do
    a <- ask
    b <- local (+ 1) $ do
        b' <- ask
        return b'
    c <- ask
    return (a, b, c)

test_func = do
    a <- (+ 1)
    b <- (* 2)
    return (a, b)

main = do
    print $ test 1
    print $ test' 1
    print $ test_func 5
