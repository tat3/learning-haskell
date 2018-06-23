
insert x [] = [x]
insert x (y:ys)
  | x < y     = x : y : ys
  | otherwise = y : insert x ys

isort []     = []
isort (x:xs) = insert x (isort xs)

qsort [] = []
qsort (n:xs) = qsort lt ++ [n] ++ qsort gteq
  where
    lt = [x | x <- xs, x < n]
    gteq = [x | x <- xs, x >= n]

fact 0 = return 1
fact n 
  | n > 0 = do
    prev <- fact (n - 1)
    return $ n * prev

main = do
  print $ isort [4,6,9,8,3,5,1,7,2]
  print $ qsort [4,6,9,8,3,5,1,7,2]
  print =<< fact 5
