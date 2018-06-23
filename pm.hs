instance Int where
    a x = x + 1

instance Str where
    a x = "hoge " ++ x

main = do
    print a 1
