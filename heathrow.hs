data Section = Section { get :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

-- map types
heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0
                   ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

-- algorithm
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwardTimeToA = timeA + a
        forwardTimeToB = timeB + b
        crossTimeToA = timeB + b + c
        crossTimeToB = timeA + a + c
        newPathToA = if forwardTimeToA <= crossTimeToA
            then (A, a) : pathA
            else (C, c) : (B, b) : pathB
        newPathToB = if forwardTimeToB <= crossTimeToB
            then (B, b) : pathB
            else (C, c) : (A, a) : pathA
    in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (pathA, pathB) = foldl roadStep ([], []) heathrowToLondon
    in if sum (map snd pathA) <= sum (map snd pathB)
        then reverse pathA else reverse pathB
    
-- manage IO
groupOf :: Int -> [a] -> [[a]]
groupOf 0 _ = undefined
groupOf _ [] = []
groupOf n xs = take n xs : groupOf n (drop n xs)



main = do
    -- print $ optimalPath heathrowToLondon
    contents <- getContents
    let threes = groupOf 3 (map read $ lines contents)
        roadSystem = map (\[x, y, z] -> Section x y z) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathTime = sum (map snd path)
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime
