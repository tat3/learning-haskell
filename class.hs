class Eq' a where
    (==.) :: a -> a -> Bool
    (/=.) :: a -> a -> Bool
    x ==. y = not (x /=. y)
    x /=. y = not (x ==. y)

data TrafficLight = Red | Yellow | Green
instance Eq' TrafficLight where
    Red ==. Red = True
    Green ==. Green = True
    Yellow ==. Yellow = True
    _ ==. _ = False

main = do
    print $ Red ==. Green
    print $ Red ==. Red
