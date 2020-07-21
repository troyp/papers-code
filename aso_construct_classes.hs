{-# LANGUAGE FlexibleInstances #-}

-- ************************************
-- *                                  *
-- * A SYSTEM OF CONSTRUCTOR CLASSES. *
-- *           Mark Jones             *
-- *                                  *
-- ************************************

-- 1. An overloaded map function.
-- ------------------------------
-- Consider the well-known map function...
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- Map obeys the following 2 laws...
-- 1. id is the fixpoint of map:           map id = id
-- 2. map distributes over composition:    map (f . g) = map f . map g
{-  In CT terms these rules say that there is functor from types to types whose object part
    maps any given type a to the list type [a] abd whose arrow part maps the function f::a->b
    to the function map f::[a]->[b]                                                        -}

-- There are similar constructions for a wide range of datatypes...
data Tree a = Leaf a | Tree a :^: Tree a
mapTree :: (a->b) -> (Tree a -> Tree b)
mapTree f (Leaf x) = Leaf (f x)
mapTree f (l :^: r) = (mapTree f l) :^: (mapTree f r)

data Opt a = Jst a | Nothg
mapOpt :: (a->b) -> (Opt a -> Opt b)
mapOpt f (Jst x) = Jst (f x)
mapOpt _ Nothg = Nothg

{-  A more attractive solution would be to have a single overloaded map function which uses
    the type of its args to determine the particular map function required.
    However, standard HM type system does not support this. Also, if map were to be as general
    as possible, we would need a mechanism to allow the definition to be extended to embrace
    new data types defined in other program modules.                                       -}

{-  Haskell supports both parametric polymorphism and overloading based on a system of type
    classes. Although each overloaded operator requires a seperate definition for each arg
    type, these need not be in the same module.
    Type classes in Haskell can be thought of as sets of types. The elements of a type class
    are defined by a collection of instance declarations which may be distributed across a
    number of program modules.
    Only a single defintion is needed for functions defined either directly or indirectly in
    terms of overloaded primitives.
    However, type classes are not sufficiently powerful to give a satisfactory treatment for
    the map function.                                                                      -}
{-  To deal with map, we'd need a class Map and a type expression m(t) such that
    S = { m(t) | t `elem` Map} includes the appropriate types, including...
    (a->b) -> [a] -> [b]
    (a->b) -> Tree a -> Tree b
    (a->b) -> Opt a -> Opt b
    for arbitrary types a,b.
    The only possiblity is to take m(t)=t and choose Map as the set of types S for which the
    map function is defined                                                                -}
class Map t where
    gmap :: t
instance Map ((a->b) -> ([a]->[b])) where
    gmap _ [] = []
    gmap f (x:xs) = f x : gmap f xs
instance Map ((a->b) -> (Tree a -> Tree b)) where
    gmap f (Leaf x) = Leaf (f x)
    gmap f (l :^: r) = (gmap f l) :^: (gmap f r)
instance Map ((a->b) -> (Opt a -> Opt b)) where
    gmap _ Nothg = Nothg
    gmap f (Jst x) = Jst (f x)
