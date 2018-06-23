import Control.Monad

main = do
  print $ join $ 
    forM [1..4] $ \x ->
    forM "abc" $ \y ->
    return (x, y)
