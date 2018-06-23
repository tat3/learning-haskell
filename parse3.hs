import Text.Parsec

test9 = sequence [char 'a', char 'b']
    <|> sequence [char 'a', char 'c']

test10 = try (sequence [char 'a', char 'b'])
    <|> sequence [char 'a', char 'c']

test13 = sequence [char 'a', char 'b' <|> char 'c']

main = do
    parseTest test9 "ab"
    parseTest test10 "ac"
    parseTest test13 "ac"
