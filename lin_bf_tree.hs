-- ************************************************
-- *                                              *
-- *  Linear-Time Breadth-First Tree Algorithms.  *
-- *  ------------------------------------------  *
-- *               Jones & Gibbons                *
-- *                                              *
-- ************************************************

data List a = Nil
            | a :-: List a
              deriving (Show, Eq)
data Stream a = a :=: Stream a

tails :: [a] -> [[a]]
tails [] = [[]]
tails l@(x:xs) = l : tails xs
