f :: Maybe Int -> Maybe Int
f (Just n) = Just $ n * 2
f Nothing = Nothing

f' :: Maybe Int -> Maybe Int
f' = fmap (* 2)

g :: Maybe Int -> Int
g (Just n) = n * 2
g Nothing = 0

g' :: Maybe Int -> Int
g' = maybe 0 (* 2)

fb :: Int -> String
fb n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 3  == 0 = "Fizz"
    | n `mod` 5  == 0 = "Buzz"
    | otherwise       = show n

fb' :: Int -> String
fb' = either id show . aux

aux :: Int -> Either String Int
aux = \n -> foldl (>>=) (return n)
    . map (\(pred, x) -> \n -> if pred n then (Left x) else (Right n))
    $ [
        (\n -> n `mod` 15 == 0, "FizzBuzz")
      , (\n -> n `mod` 3  == 0, "Fizz")
      , (\n -> n `mod` 5  == 0, "Buzz")
      ]


