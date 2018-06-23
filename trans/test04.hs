{-# LANGUAGE InstanceSigs #-}

import System.Exit (die)
import Text.Read (readMaybe)

readInt :: String -> Maybe Int
readInt = readMaybe

newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where
    fmap :: (a -> b) -> MaybeIO a -> MaybeIO b
    fmap f action = MaybeIO $ do
        x <- runMaybeIO action
        case x of
            Nothing -> pure Nothing
            Just x' -> pure (Just (f x'))

instance Applicative MaybeIO where
    pure :: a -> MaybeIO a
    pure x = MaybeIO (pure (Just x))

    (<*>) :: MaybeIO (a -> b) -> MaybeIO a -> MaybeIO b
    lhs <*> rhs = MaybeIO $ do
        mf <- runMaybeIO lhs
        case mf of
            Nothing -> pure Nothing
            Just f -> do
                mx <- runMaybeIO rhs
                case mx of
                    Nothing -> pure Nothing
                    Just x -> pure (Just (f x))

instance Monad MaybeIO where
    (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
    lhs >>= rhs = MaybeIO $ do
        x <- runMaybeIO lhs
        case x of
            Nothing -> pure Nothing
            Just x' -> runMaybeIO (rhs x')
            


main :: IO ()
main = do
    msum <- runMaybeIO $ do
        a <- MaybeIO (readInt <$> getLine)
        b <- MaybeIO (readInt <$> getLine)
        pure (a + b)
    case msum of
        Nothing -> die "not an integer"
        Just sum -> print sum
