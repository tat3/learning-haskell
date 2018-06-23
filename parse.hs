import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.String

run :: Show a => Parser a -> String -> IO ()
run p input =
    case parse p "hoge" input of
        Left err ->
            putStr "parse error at" >> print err
        Right x -> print x

openClose :: Parser String
openClose = do
    a <- char '('
    b <- char ')'
    return [a, b]

a_b_c :: Parser Char
a_b_c = char 'a' <|> char 'b' <|> char 'c'

parens :: Parser String
parens = do
    a <- char '('
    b <- parens
    c <- char ')'
    return $ a : b ++ [c]
    <|>
    return []

parens' = char '(' >>= \a -> parens >>= \b -> char ')' >>= \c -> return (a : b ++ [c]) <|> return []

main = do
    run (oneOf "abc") "aaa"
    run (noneOf "abc") "zabc"
    run openClose "()"
    run openClose "(4)"
    run a_b_c "a"
    run parens' "(())"
