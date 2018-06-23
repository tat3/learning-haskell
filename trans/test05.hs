import System.Exit (die)
import Text.Read (readMaybe)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))

readInt :: String -> Maybe Int
readInt = readMaybe

readMaybeInt :: IO (Maybe Int)
readMaybeInt = readMaybe <$> getLine

main :: IO ()
main = do
    msum <- runMaybeT $ do
        a <- MaybeT readMaybeInt
        b <- MaybeT readMaybeInt
        c <- MaybeT readMaybeInt
        d <- MaybeT readMaybeInt
        pure (a + b + c + d)
    case msum of
        Nothing -> die "not an integer"
        Just sum -> print sum
