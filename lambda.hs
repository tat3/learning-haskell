map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' f [] = []
filter' f (x:xs) = 
  if f x then x : filter' f xs else filter' f xs

flip' f x y = f y x

foldl' f n [] = n
foldl' f n (x:xs) = foldl' f (f n x) xs

foldr' f n [] = n
foldr' f n (x:xs) = f n $ foldr' f n xs

main = do
  print $ map' (* 2) [1..5]
  print $ filter' (< 5) [1..9]
  print $ flip' map' [1..5] (* 2)
  print $ foldl' (+) 0 [1..100]
  print $ foldl' (-) 0 [1..5]
  print $ foldr' (-) 0 [1..5]
