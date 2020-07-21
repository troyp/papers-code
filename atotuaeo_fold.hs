-- *****************************************************************
-- *                                                               *
-- *   A TUTORIAL ON THE UNIVERSALITY AND EXPRESSIVENESS OF FOLD   *
-- *                                                               *
-- *                     Graham Hutton.                            *
-- *                                                               *
-- *****************************************************************


-- 1. Introduction.
-- ----------------

{-  Many programs involving repetition use some form of recursion, and properties of such
    programs are proved using some form of induction. Such programs and proofs often repeat
    the same patterns, which are captured as "recursion operators" and "proof pricnciples". -}

{- In FP, fold is a standard recursion operator encapsulating a common pattern of recursion for
   processing lits. It comes equipped with a proof principle called "universality", which
   encapsulates a common pattern of inductive proof concerning lists.
   Fold and its universal property together form the basis of a simple but powerful calculational
   theory of programs that process lists.
   This theory generalises to a range of other datatypes, but we'll restrict our attention to
   lists for simplicity.                                                                    -}

{- We focus on two key aspects of fold...
   1. We emphasise the use of the universal property of fold (together with the derived "fusion"
      property), both as *proof principles* that avoid the need for inductive proofs, and as
      *definition principles* that guide the transformation of recursive programs into
      definitions involving fold.
   2. We show that although the pattern of recursion captured by fold is simple, in a language
      with tuples and functions as first-class values, the fold operator has greater expressive
      power than might be expected, thus permitting the powerful universal and fusion properties
      of fold to be applied to a larger class of programs.                                    -}


-- The fold Operator.
-- ------------------

-- the fold operator has its roots in recursion theory and has been used in PLs since APL and FP.

fold :: (a -> b -> b) -> b -> ([a] -> b)
fold f v [] = v
fold f v (x:xs) = f x (fold f v xs)
{-  note that the parens on the right in the type sig are superfluous, but indicate our
    perspective in treating fold as a function-returning function.                    -}

{- Given a function f::a->b->b, the function (fold f v) processes a list of type [a] to produce
   a value of type b by replacing the nil constructor with the value v, and each cons constructor
   (:) within the list by the function f.
   Thus, fold encapsulates a simple pattern of recursion for processing lists, in which the two
   constructors for lists are simply replaced by other values and functions.                  -}

-- many common functions have simple definitions using fold...
sum' :: [Int] -> Int
sum' = fold (+) 0
product' :: [Int] -> Int
product' = fold (*) 1
and' :: [Bool] -> Bool
and' = fold (&&) True
or' :: [Bool] -> Bool
or' = fold (||) False
(+++) :: [a] -> [a] -> [a]
(+++) xs ys = fold (:) ys xs
length' :: [a] -> Int
length' = fold (\x n -> n+1) 0
-- or...
length'' = fold (const (+1)) 0
reverse' :: [a] -> [a]
reverse' = fold (\x xs -> xs ++ [x]) []
map' :: (a -> b) -> ([a] -> [b])
map' f = fold (\x xs -> f x : xs) []
filter :: (a -> Bool) -> ([a] -> [a])
filter p = fold (\x xs -> if p x then x:xs else xs) []

{- Programs written using fold can be less readable than those written using explicit recursion,
   but can be constructed systematically and are better suited to transformation and proof.
   For instance, we'll see later how the above defintion of map can be constructed from the
   standard defn and how it simplifies the process of proving properties of the map function. -}


-- 3. The Universal Property of fold.
-- ----------------------------------

{- The universal property of fold, like the operator itself, has its roots in recursion theory.
   Its first use in FP was Malcolm's use of it in his generalisation of the Bird-Meerten theory
   of lists to arbitrary regular datatypes.                                                  -}

{- For finite lists, the universal property of fold can be stated as the following equivalence
   between two definitions for a list-processing function g...

   1.   g []     = v                                    2.   g = fold f v
        g (x:xs) = f x (g xs)          <===>                
   
   In the right-to-left direction, substituting  g = fold f v  into the equations gives the
     recursive defn of fold.
   In the left-to-right direction, the two equations are precisely the assumptions needed to
     prove that  g = fold f v  using a simple inductive proof on finite lists
     (see Bird, An introduction to functional programming using Haskell).
   Taken as a whole, the universal property states that  fold f v  is not just a solution to
     the defining equations, but the unique solution.                                     -}

{- The utility of the universal property is that it makes explicit the two assumptions required
     for a certain pattern of inductive proof. For specific cases, we can avoid the need for
     induction by verifying the two assumptions and then appealing to the universal property.
   Thus, the universal property encapsulates a simple pattern of inductive proof concerning
     lists, just as the fold operator itself encapsulates a simple pattern of recursion for
     processing lists.
   The universal property can be generalised to handle partial and infinite lists (see Bird),
     but for simplicity we only consider finite lists here.                                 -}


-- 3.1. Universality as a Proof Principle.
-- ---------------------------------------
