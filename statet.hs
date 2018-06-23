import Control.Monad.Identity
import Control.Monad.State

test1 = do
    let a = return 1 :: StateT s Identity Int
    print $ evalState a ()

st = return 1 :: State s Int

f1 :: s -> (Int, s)
f1 = runState st

f2 :: s -> Identity (Int, s)
f2 = runStateT st

test2 = do
    print $ f1 ()
    print $ runIdentity $ f2 ()

return' x = StateT $ \s -> Identity (x, s)
runState' st = runIdentity . runStateT st


main = do
    let st = return' 1
    print $ runState' st ()
