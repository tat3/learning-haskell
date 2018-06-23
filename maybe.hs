bind Nothing _ = Nothing
bind (Just x) f = f x

main = do
    print $ Just 1 `bind` \a -> Just $ a * 2
    print $ Just 1 `bind` \a -> Nothing `bind` \b -> Just $ a * b

