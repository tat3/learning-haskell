import Text.Parsec
import Control.Applicative ((<$>), (<*>), (*>), (<*))

eval m fs = foldl (\x f -> f x) <$> m <*> fs
apply f m = flip f <$> m

expr = eval term $ many $
        char '+' *> apply (+) term
    <|> char '-' *> apply (-) term

term = eval factor $ many $
        char '*' *> apply (*) factor
    <|> char '/' *> apply div factor

factor = spaces *> (char '(' *> expr <* char ')' <|> number) <* spaces

number = read <$> many1 digit

main = do   
    parseTest number "123"
    parseTest expr "1+2+3"
    parseTest expr "1-2+3"
    parseTest expr "2+4/2"
    parseTest expr "(2+4)/2"
    parseTest expr "( 2 + 3 ) * 5"
