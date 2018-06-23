import System.Exit (die)
import Text.Read (readMaybe)

readInt :: String -> Maybe Int
readInt = readMaybe

main :: IO ()
main = do
    a <- readInt <$> getLine
    case a of 
        Nothing -> die "not an integer"
        Just a' -> do
            b <- readInt <$> getLine
            case b of
                Nothing -> die "not an integer"
                Just b' -> print (a' + b')
