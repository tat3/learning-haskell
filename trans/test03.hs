safeDiv m n 
    | n == 0 = Nothing
    | otherwise = Just (div m n)

hoge :: Maybe Int
hoge = case safeDiv 42 2 of
    Nothing -> Nothing
    Just a -> case safeDiv a 0 of
        Nothing -> Nothing
        Just b -> case safeDiv b 3 of
            Nothing -> Nothing
            Just c -> safeDiv c 7

hoge' = safeDiv 42 2
    >>= \a -> safeDiv a 0
    >>= \b -> safeDiv b 3
    >>= \c -> safeDiv c 7

hoge'' = do
    a <- safeDiv 42 2
    b <- safeDiv a 0
    c <- safeDiv b 3
    safeDiv c 7


main :: IO ()
main = do
    print $ hoge
