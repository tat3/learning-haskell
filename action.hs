import System.Random
import Control.Applicative

randAlpha = getStdRandom $ randomR ('a', 'z')

fact 0 = return 1
fact n | n > 0 = do
  prev <- fact (n - 1)
  return $ n * prev

shuffle [] = return []
shuffle xs = do
  n <- getStdRandom $ randomR (0, length xs - 1)
  xs' <- shuffle $ take n xs ++ drop (n + 1) xs
  return $ (xs !! n) : xs'

isSorted [] = True
isSorted [x] = True
isSorted (x:y:zs)
  | x > y = False
  | otherwise = isSorted (y : zs)

bogo l = do
  x <- shuffle l
  if isSorted x then return x else bogo l

inc x = x + 1
add x y = x + y
incAction x = return $ x + 1
main = do
  print =<< inc <$> return 1
  print =<< add <$> return 1 <*> return 2
  print =<< incAction =<< return 1
