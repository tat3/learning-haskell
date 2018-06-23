import Control.Monad.State
import Data.List
import System.Random


type Card = Int
type Score = Int
type Hand = [Card]
type Stock = [Card]
type Player = String

game :: [Card] -> [(Score, Hand, Player)]
game deck = let
    (taroHand, deck2) = (take 5 deck, drop 5 deck)
    (hanakoHand, deck3) = (take 5 deck2, drop 5 deck2)
    (takashiHand, deck4) = (take 5 deck3, drop 5 deck3)
    (yumiHand, deck5) = (take 5 deck4, drop 5 deck4)
    in reverse . sort $
        [ (sum taroHand, taroHand, "Taro")
        , (sum hanakoHand, hanakoHand, "Hanako")
        , (sum takashiHand, takashiHand, "Takashi")
        , (sum yumiHand, yumiHand, "Yumi")
        ]

drawCards :: Int -> State Stock Hand
drawCards n = do
    deck <- get
    put $ drop n deck
    return $ take n deck

game_state :: State Stock [(Score, Hand, Player)]
game_state = do
    taroHand <- drawCards 5
    hanakoHand <- drawCards 5
    takashiHand <- drawCards 5
    yumiHand <- drawCards 5
    return . reverse . sort $
        [ (sum taroHand, taroHand, "Taro")
        , (sum hanakoHand, hanakoHand, "Hanako")
        , (sum takashiHand, takashiHand, "Takashi")
        , (sum yumiHand, yumiHand, "Yumi")
        ]

shuffle [] = return []
shuffle xs = do
    n <- getStdRandom $ randomR (0, length xs - 1) :: IO Int
    xs' <- shuffle $ take n xs ++ drop (n + 1) xs
    return $ (xs !! n) : xs'   

showResult xs = do
    forM xs $ \i -> do
        print i

stock :: Stock
stock = [1..50]

runGame = do
    deck <- shuffle stock
    showResult $ fst $ runState game_state deck

main = do
    runGame
