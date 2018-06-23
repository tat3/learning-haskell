import Data.Char
import Control.Monad.State
import Control.Monad
import Control.Applicative ((<$>), (<*>))


parseTest p s = case evalStateT p s of
    Right r -> print r
    Left e -> putStrLn $ "[" ++ show s ++ "] " ++ e

anyChar = StateT $ anyChar where
    anyChar (x:xs) = Right (x, xs)
    anyChar _ = Left "too short"

satisfy f = StateT $ satisfy where
    satisfy (x:xs) | not $ f x = Left $ ": " ++ show x
    satisfy xs = runStateT anyChar xs

(StateT a) <|> (StateT b) = StateT $ \s ->
    (a s) <|> (b s) where
        Left a <|> Left b = Left $ b ++ a
        Left _ <|> b = b
        a <|> _ = a

left = lift . Left

char c = satisfy (== c) <|> left ("not char " ++ show c)
digit = satisfy isDigit <|> left "not digit"
letter = satisfy isLetter <|> left "not letter"

many p = ((:) <$> p <*> many p) <|> return []

test1 = sequence [anyChar, anyChar]
test2 = (++) <$> test1 <*> sequence [anyChar]
test3 = sequence [letter, digit, digit]
test4 = letter <|> digit

test5 = sequence [letter, digit, digit, digit]
test6 = sequence $ letter : replicate 3 digit

test7 = many letter
test8 = many test4

main = do
    parseTest (satisfy (== 'a')) "abc"
    parseTest digit "123"
    parseTest test3 "a23"
    parseTest test7 "abc123"
    parseTest test8 "123def"
