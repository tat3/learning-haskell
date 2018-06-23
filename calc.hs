solveRPN :: String -> Double
solveRPN expression = head (foldl foldingFunction [] (words expression))
    where
        foldingFunction :: [Double] -> String -> [Double]
        foldingFunction (x:y:ys) "*" = (x * y) : ys
        foldingFunction xs numberString = read numberString : xs

main = do
    print $ "3 5 * 5 *"
    print $ solveRPN "3 5 * 5 *"
    -- print $ sum' [1,2,3]

