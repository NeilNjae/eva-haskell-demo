-- Advent: days 1, 6, 8
-- parsing: 14, 22

module Earlier where

import qualified Data.Tree as DT
import Data.List 

toDataTree :: (Show a) => (Tree a) -> (DT.Tree String)
toDataTree (Leaf a) = DT.Node (show a) []
toDataTree (Branch b cs ds) = DT.Node (show b) [toDataTree cs, toDataTree ds]



data Tree a = Branch a (Tree a) (Tree a) 
              | Leaf a deriving (Eq,Ord,Show)


instance Functor Tree where
    fmap = mapTree


instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Branch x l r) = (foldMap f l) <> (f x) <> (foldMap f r) 


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch x left right) = 
    Branch (f x) 
           (mapTree f left)
           (mapTree f right)





strTree :: Tree String
strTree = Branch "1" (Branch "11" (Leaf "111") (Leaf "112")) 
               (Branch "12" (Leaf "121") (Leaf "122"))

intTree :: Tree Int
intTree = 
    Branch 12
        (Branch 
            7
            (Branch 
                5
                (Leaf 1)
                (Leaf 6)
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


ppt :: (Show a) => Tree a -> IO ()
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

fibsDirect = fibsNaiveHelper 0
    where fibsNaiveHelper n = (fibNaive n) : (fibsNaiveHelper (n + 1))


names :: [String]
names = ["alice", "bob", "eve"]

suffs :: [String]
suffs =  ["!", "?", " :-)"]


fibsNaive = map fibNaive [0..]


-- 0  1  1  2  3  5  8 13 21 34  fibs
-- 1  1  2  3  5  8 13 21 34     tail fibs
-- 1  2  3  5  8 13 21 34        zipWith (+)


fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- “The mind, once stretched by a new idea, never returns to its original dimensions.”
-- ― Ralph Waldo Emerson


-- fib :: Int -> Int
-- fib n = genericIndex fibs n
fib n = fibs!!n


sumFirstFibs howMany = sumList nums
    where nums = take howMany fibs
          sumList [] = 0
          sumList (n:ns) = n + sumList ns


fibsU :: [Int]
fibsU = unfoldr (\(a, b) -> Just (a, (b, a+b))) (0, 1)

fibsULimit :: Int -> [Int]
fibsULimit limit = 
    unfoldr 
        (\(a, b) -> if a > limit 
                    then Nothing 
                    else Just (a, (b, a+b))) 
        (0, 1)






type Name = String
type Key = String
type Address = String
type Cost = Int



keyFromName :: Name -> Key
keyFromName name = name ++ "'s key"

addressFromKey :: Key -> Address
addressFromKey key = key ++ "'s address"

postCostFromAddress :: Address -> Cost
postCostFromAddress address = length address

addressFromName = addressFromKey . keyFromName

postCostFromName = postCostFromAddress . addressFromKey . keyFromName


maybeKeyFromName :: Name -> Maybe Key
maybeKeyFromName name 
    | name == "neil" = Just (name ++ "'s key")
    | otherwise      = Nothing

maybeAddressFromKey :: Key -> Maybe Address
maybeAddressFromKey key = Just (key ++ "'s address")

maybePostCostFromAddress :: Address -> Maybe Cost
maybePostCostFromAddress address = Just (length address)


-- maybeAddressFromName name = 
--     case (maybeKeyFromName name) of
--         Just key -> maybeAddressFromKey key
--         Nothing -> Nothing

-- maybePostCostFromName name = 
--     case (maybeKeyFromName name) of
--         Just key -> case (maybeAddressFromKey key) of
--                         Just address -> maybePostCostFromAddress address
--                         Nothing -> Nothing
--         Nothing -> Nothing


composeMaybe :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
composeMaybe f g = \x -> case (g x) of
                            Just y -> f y
                            Nothing -> Nothing

maybePostCostFromName = maybePostCostFromAddress `composeMaybe` maybeAddressFromKey `composeMaybe` maybeKeyFromName

(<=<) = composeMaybe

mpcfn = maybePostCostFromAddress <=< maybeAddressFromKey <=< maybeKeyFromName
