-- Advent: days 1, 6, 8

module Main where

import qualified Data.Tree as DT
import Data.List 

data Tree a = Branch a (Tree a) (Tree a) 
              | Leaf a deriving (Eq,Ord,Show)


instance Functor Tree where
    fmap = mapTree

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch x left right) = 
    Branch (f x) 
           (mapTree f left)
           (mapTree f right)

toDataTree :: (Show a) => (Tree a) -> (DT.Tree String)
toDataTree (Leaf a) = DT.Node (show a) []
toDataTree (Branch b cs ds) = DT.Node (show b) [toDataTree cs, toDataTree ds]


strTree :: Tree String
strTree = Branch "1" (Branch "11" (Leaf "111") (Leaf "112")) 
               (Branch "12" (Leaf "121") (Leaf "122"))

intTree :: Tree Int
intTree = 
    Branch 12
        (Branch 
            5
            (Branch 
                3
                (Leaf 1)
                (Leaf 7)
            )
            (Leaf 10)
        )
        (Branch 
            21
            (Leaf 17)
            (Branch
                23
                (Leaf 22)
                (Branch 
                    27
                    (Leaf 24)
                    (Leaf 29)
                )
            )
        )


ppt :: (Show a) => (Tree a) -> (IO ())
ppt = putStrLn . DT.drawTree . toDataTree


main :: IO ()
main = do
  putStrLn "hello world"
  ppt strTree
  ppt intTree


fibNaive :: Int -> Int
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)

fibsDirect = fibsDirectHelper 0
    where fibsDirectHelper n = (fibNaive n) : (fibsDirectHelper (n + 1))


fibsNaive = map fibNaive [0..]


fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- fib :: Int -> Int
-- fib n = genericIndex fibs n
fib n = fibs!!n


fibsU :: [Int]
fibsU = unfoldr (\(a, b) -> Just (a, (b, a+b))) (0, 1)

fibsULimit :: Int -> [Int]
fibsULimit limit = unfoldr (\(a, b) -> if a > limit then Nothing else Just (a, (b, a+b))) (0, 1)


names :: [String]
names = ["alice", "bob", "eve"]

suffs :: [String]
suffs =  ["!", "?", " :-)"]
