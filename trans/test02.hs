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

main :: IO ()
main = do
    print $ hoge
