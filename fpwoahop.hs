
{-# LANGUAGE FlexibleInstances, RankNTypes, TypeSynonymInstances, UndecidableInstances
  , OverlappingInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Fpwoahop where

import Data.List
import Control.Monad hiding (join)


--  *********************************************
--  *                                           *
--  *  Functional Programming with Overloading  *
--  *      and Higher-Order Polymorphism        *
--  *              Mark P Jones.                *
--  *                                           *
--  *********************************************

-- ==================================
-- 2. THE HINDLEY-MILNER TYPE SYSTEM.
-- ==================================

-- In HM, the most general type of a term can be inferred without annotations
-- in some cases the most general type is monomorphic...
not' :: Bool -> Bool
not' False = True
not' True = False
-- in other cases the most general type is polymorphic...
identity :: a -> a
identity x = x
-- the type variable a above represents an arbitrary type. Another example...
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs
{- there is an implicit "forall" quantifier at the beginning of a parametrized
   type declaration                -}


-- ================
-- 3. Type Classes.
-- ================

{-  The HM type system is convenient for many functions, but there are important
    functons that cannot be expressed in terms of it. For example, we would
    like to be able to use the addition operator to add ints, floating point
    numbers, or other numeric types, but a polymorphic type is no good, since
    addition doesn't make sense for non-numeric types.
    Another example is the equality operator. If we use a polymorphic type,
    then equality could be used to test fuction types, which is not allowable
    (since there is no computable equality for functions).
    In these cases monomorphic types are too restrictive, but polymorphic types
    are too general. We need something intermediate between the two, allowing
    definition over a range of types, but not all types.
    Type Classes provide a soluation.                -}

{-  The Prelude contains a number of type classes, which can be thought of as
    sets of types whose members are called "instances". Examples of type classes
    are Eq, Ord, Num and Show. The prelude also contains a number of functions
    of restricted polymorphic type.
(==) :: Eq a => a -> a -> Bool
min :: Ord a => a -> a -> a
show :: Show a => a -> String
(+) :: Num a => a -> a -> a
    Class constraints may also appear in the types of user-defined functions
    which directly or indirectly make use of prelude functions with restricted
    polymorphic types. For example...                -}
member :: Eq a => [a] -> a -> Bool
member xs x = any (x==) xs
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (member ys) xs    -- = all . elem
-- we can extend some classes to include user-defined types using "deriving"
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
         deriving (Eq, Ord, Show)
{-  in the general case, a derived instance may require a context. For example,
    the datatype definition...
data Either a b = Left a | Right b  deriving (Eq, Ord)
    will result in two derived instances...
instance (Eq a, Eq b) => Eq (Either a b)
instance (Ord a, Ord b) => Ord (Either a b)

    Instances can be derived for Eq, Ord, Enum, Show, Read and Bounded.
    To define an instance of another class, or if automatic derivation creates
    inappropriate semantics, we must provide our own implementation of required
    functions.                -}
{-  For example, suppose we create a Set datatype implemented using lists
    (which may contain (ignored) duplicates).
data Set a = Set [a] deriving (Eq)
    This will cause (==) to test equality of lists, not equality of sets.
    Looking at the definition of type class Eq...
class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x==y)
    -- Note that a default definition for (/=) in terms of (==) is provided.
    So we need to provide our own definition of (==) for Set                -}
data Set a = Set [a] deriving (Show)
instance Eq a => Eq (Set a) where
    Set xs == Set ys  =  subset xs ys && subset ys xs
-- this gives the intended semantics of set equality.
{-  Note that a type class can be arbitrarily extended to include new instances
    without altering the original class definition - this gives a high degree of
    mosularity and extensibility.                -}

-- Defining a new class.
-- ---------------------
{-  In defining a new class, we must decide what common properties we expect
    instances to share and how this is to be reflected in the operators included
    in the class definition.
    Note that overloading is only appropriate if the meaning of a symbol is
    uniquely determined by the types of its operands.
    For example, the following might be considered an abuse of the system since
    monoids are not uniquely determined by type...                -}
class Monoid a where
    e :: a
    op :: a -> a -> a
instance Monoid [a] where
    e = []
    op = (++)
instance Monoid (a->a) where
    e = id
    op = (.)
instance Monoid Int where
    e = 0
    op = (+)
{-  The final instance declaration is particularly difficult to justify:
    there is an equally natural way to define a monoid structure on Ints with
    e = 1, op = (*).
    Ideally, an instance declaration should involve a single natural 
    implementation for an overloaded operator.                -}

-- Trees.
-- ------
-- There are numerous types of tree. For example...
-- Simple binary trees with a value of type a at each leaf node...
data BinTree a = Leaf a
               | BinTree a :^: BinTree a
               deriving (Eq, Show)
exBintree = (Leaf 1 :^: Leaf 2) :^: (Leaf 3 :^: Leaf 4)
-- Labelled trees with a value of type a at each leaf node and a value
-- of type l at each interior node...
data LabTree l a = Tip a
                 | LFork l (LabTree l a) (LabTree l a)
                 deriving (Eq, Show)
{- Binary search trees with a value of type a in the body of the tree.
   These are typically used for searching for an element in the tree using
   an ordering on the values of type a...                -}
data STree a = Empty
             | Split a (STree a) (STree a)
             deriving (Eq, Show)
-- Rose trees in which each node is labelled with a value of type a and may
-- have an arbitrary number of subtrees...
data RoseTree a = Node a [RoseTree a] deriving (Eq, Show)
exRosetree = Node 1 [(Node 2 []) , (Node 3 [(Node 4 []) , (Node 5 [])]) , (Node 6 [])]
{- Abstract syntax: eg. a datatype to represent lambda expressions where leaf
   nodes correspond to variables and interior nodes are either applications or
   abstractions...                -}
type Name = String
data Term = Var Name
          | Ap Term Term
          | Lam Name Term
          deriving (Eq, Show)
exAst = Lam "f" (Ap (Ap (Var "f") (Var "x")) (Var "y"))
{-  There are strong similarities between these various trees, and many concepts
    apply equally to all, eg. depth, size, paths, subtrees, etc.
    By defining a type class for tree-like datatypes, we can write a generic
    function for day, calculating the depth of a tree, that will work with any
    instance.                -}
-- we define a tree class by observing that every tree has a number of subtrees
class Tree t where
    subtrees :: t -> [t]    -- generates the list of proper subtrees of a tree
-- instantiations of our tree types...
instance Tree (BinTree a)where
    subtrees (Leaf n) = []
    subtrees (left :^: right) = [left, right]
instance Tree (LabTree l a) where
    subtrees (Tip x) = []
    subtrees (LFork x left right) = [left, right]
instance Tree (STree a) where
    subtrees Empty = []
    subtrees (Split x left right) = [left, right]
instance Tree (RoseTree a) where
    subtrees (Node x gts) = gts
instance Tree Term where
    subtrees (Var _) = []
    subtrees (Ap f x) = [f, x]
    subtrees (Lam v b) = [b]
-- we can now construct functions to act on any tree that has been instantiated
depth :: Tree t => t -> Int
depth = (1+) . foldl max 0 . map depth . subtrees
size :: Tree t => t -> Int
size = (1+) . sum . map size . subtrees
{-  Of course, there are more efficient implementations of these functions for
    specific tree types. However, it's quite possible that a compiler could
    automatically obtain the more efficient implementation by generating
    specialized versions of the overloaded operators (for example).    -}
{-  Another example of a generic algorithm is calculaing the list of paths from
    the root node to each leaf. In a particular tree, we may represent this by a
    sequence of labels, or of directions such as "left" and "right". However,
    such methods are not generally applicable, so we'll identify each path with
    the corresponding sequence of subtrees.                -}
paths :: Tree t => t -> [[t]]
paths t | null sts = [[t]]
        | otherwise = [t:p | b<-sts, p<-paths b]
        where sts = subtrees t
-- version using list functions instead of list comprehensions
paths' t | null sts = [[t]]
         | otherwise = map (t:) . concat . map paths $ sts
         where sts = subtrees t
-- we can also express depth-first and breadth-first search, yielding lists of
-- subtrees in the appropriate order.
dfs :: Tree t => t -> [t]
dfs t = t : concat (map dfs (subtrees t))
{-  This approach to a depth-first search is well-suited to a lazy language, where the
    resulting list may not be fully evlauated. For example, if p is a preducate on trees,
    we may use the following function to find the first node in a depth-first search to
    satisfy p, and once it has been found, the search will terminate.                 -}
findNode :: (Tree a) => (a -> Bool) -> a -> a
findNode p = head . filter p . dfs

{- breadth-first search: 
   reference: Jones & Gibbons - Linear-time breadth-first tree algorithms: an exercise in the
                                arithmetic of folds and zips.                              -}
bfs :: Tree t => t -> [t]
bfs = concat . lev
    where lev t = [t] : foldr cat [] (map lev (subtrees t)) -- list of lists: one "level"
          cat = combine (++)    -- combine 2 lists of lists into a single list of lists

-- combine "lifts" a binary function into a binary list function
combine :: (a -> a -> a) -> ([a] -> [a] -> [a])
combine f (x:xs) (y:ys) = f x y : combine f xs ys
combine f [] ys = ys
combine f xs [] = xs

{-  As a final example, we consider a function to draw character-based diagrams of
    arbitrary tree values. We might use such a dunction, for example, to visualise the
    results of simple tree-based algorithms.                                            -}
-- the tree-drawing algorithm is based on the function...
-- drawTree' :: Tree t => (t -> String) -> t -> [String]
{-  The first argument of drawTree' is a function taking a tree and returning the label
    (if any) of its root node.
    The second argument is the tree to be drawn.
    The result is a list of strings, each corresponding to a line of output.
    The labelling function (first arg to drawTree') is problematic - each type of Tree will
    require its own function. To avoid having to hardcode the possibilities in the final
    drawTree function, or pass the labelling function to it explicitly, we will define a
    subclass of Tree that provides appropriate functions for labelling and drawing.    -}
class Tree t => DrawTree t where
 drawTree :: t -> String
 labTree  :: t -> String
 drawTree = unlines . drawTree' labTree    -- default implementation (may be overridden)
 labTree = const "@"    -- default label
instance DrawTree Term where
    labTree (Var v) = v
    labTree (Ap _ _) = "@"
    labTree (Lam v _) = "\\"++v
-- [ex] define other instances
instance Show a => DrawTree (BinTree a) where
    labTree (Leaf a) = show a
    labTree (left :^: right) = "@"
-- [ex] construct drawTree' function
pref f = ("--"++) . f    -- constructs label prefix "--label" from label func and tree
type Diagram = [String]
diaHt :: Diagram -> Int
diaHt = length
diaLn = length . head
blanks n = replicate n ' '
padLeft :: Int -> Diagram -> Diagram
padLeft w d = let w' = diaLn d
                  del = w - w'
                  pad = blanks del
              in map (pad++) d
padRight :: Int -> Diagram -> Diagram
padRight w d = let w' = diaLn d
                   del = w - w'
                   pad = blanks del
               in map (++pad) d
padBottom :: Int -> Diagram -> Diagram
padBottom h d = let width = diaLn d
                    height = diaHt d
                in d ++ replicate (h - height) (blanks width)
(<>) :: Diagram -> Diagram -> Diagram
left <> right = let height = maximum [diaHt left, diaHt right]
                    left' = padBottom height left
                    right' = padBottom height right
                in map2 (++) left' right'
map2 f [] _ = []
map2 f _ [] = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys
diaJoin :: String -> Diagram -> Diagram -> Diagram
-- takes two tree diagrams and makes a diagram of a node joining them
diaJoin label left right = let len' = maximum [diaLn left, diaLn right]
                               left' = padRight len' left
                               right' = padRight len' right
                               height' = diaHt left' + diaHt right' + 1
                               indent = length label
                               len'' = len' + indent
                           in [label ++ head left'] ++
                              (padLeft len'' (map ('|':) (tail left))) ++
                              [replicate (indent - 1) ' ' ++ "|" ++ blanks len'] ++
                              [replicate (indent - 1) ' ' ++ "'" ++ head right'] ++
                              (padLeft len'' (tail right))
drawTree' :: DrawTree t => (t -> String) -> t -> [String]
drawTree' f t = [pref labTree t] <> drawTree'' f t
drawTree'' f t = case subtrees t of
                  [] -> [pref f t]
                  (st:sts) -> foldl (diaJoin "--@")
                              (drawTree'' f st)
                              (map (drawTree'' f) sts)
-- TODO: other instances. In particular, test with LabTrees

-- Duality and the DeMorgan Principle.
-- -----------------------------------
class Dual a where
    dual :: a -> a
    -- constraint: dual . dual = id
instance Dual Bool where
    dual = not
instance (Dual a, Dual b) => Dual (a -> b) where
    dual f = dual . f . dual
{-  eg. dual (&&) = (||) and dual (||) = (&&)
        so dual (&&) False True = True                -}

{-  Inspired by the work of Turner, we extend the concept of duality and
    DeMorgan's Laws to the list datatype (considering only finite lists).                            -}
instance Dual a => Dual [a] where
    dual = reverse . map dual    -- requires list be finite
head' :: Dual a => [a] -> a
head' = dual last
last' :: Dual a => [a] -> a
last' = dual head
init'  :: Dual a => [a] -> [a]
init' = dual tail
tail' :: Dual a => [a] -> [a]
tail' = dual init
dConc :: Dual a => [a] -> [a] -> [a]
dConc = dual (++)    --    = flip (++)
-- flip is the Haskell verison of the W-combinator
dFoldr :: (Dual a, Dual b) => (a->b->b) -> b -> [a] -> b
dFoldr = dual foldr    --    = foldl . flip
dFoldl :: (Dual a, Dual b) => (b->a->b) -> b -> [a] -> b
dFoldl = dual foldl    --    = foldr . flip
exDualFold  = ( dual foldr (:) [] [1..4] ) :: [Int]
exDualFold' = ( foldl (flip (:)) [] [1..4] ) :: [Int]

-- we extend our framework to include integers with unary minus as the dual function
instance Dual Int where 
    dual = negate
plus :: (Num a, Dual a) => a -> a -> a
plus = dual (+)    --    dual (+) = (+)
max' :: (Ord a, Dual a) => a -> a -> a
max' = dual min
min' :: (Ord a, Dual a) => a -> a -> a
min' = dual max


-- Computing with Lattices.
-- ------------------------

{-  A lattice is a partially-ordered set with a top and bottom value in which every pair of
    elements has a "meet" (greatest lower bound) and "join" (least upper bound). Motivated
    by the study of "frontiers" and their use in strictness analysis, Mark Jones developed
    a framework for computing with finite lattices using typeclasses. The resulting system
    is more elegant than a previous implementation without typeclasses, which was hindered
    by the shortcomings of HM.

    The most important part of the framework is the definition of the Lattice typeclass... -}

class Eq a => Lattice a where
    bottom, top :: a                -- required
    meet, join  :: a -> a -> a      -- required
    lt          :: a -> a -> Bool
    x`lt`y =  (x`join`y)==y    -- default implementation
{-  The lt ("less than") function, here written as an infix operator, is used to describe
    the partial order. The default implementation defines it in terms of join and (==)   -}

{-  Bool provides one of the simplest examples of a lattics, with meet and join corresponding
    to conjunction and disjunction, respectively.                                           -}
instance Lattice Bool where
    bottom = False
    top    = True
    meet   = (&&)
    join   = (||)
{-  note that we ignore any improper elements of lattice types (here, just the bottom element,
    "undefined") since these values can't be used without risking abnormal or non-termination -}
{-  Other common lattice types are sets with union and intersection; numbers with min and max;
    integers with gcd and lcm                                                                -}

-- we can define the lattice structure of the product of two lattices...
instance (Lattice a, Lattice b) => Lattice (a,b) where
    bottom = (bottom, bottom)
    top    = (top,top)
    (x,y)`meet`(u,v) = (x`meet`u, y`meet`v)
    (x,y)`join`(u,v) = (x`join`u, y`join`v)
{-  It is possible to extend the Lattice class with other types of lattices: lattices of,
    subsets, frontiers or functions, lifted lattices, etc.                              -}

-- Least Fixed Point Operator.
{-  If a function f is monotonic on some lattice a, then f has a least fixed point, which  can
    be obtained as the limit of the sequence
    iterate f bottom = [ bottom, f bottom, f (f bottom), ...]
    Assuming the lattice is finite, the limit will be the first (and only) repeated value in
    the sequence.                                                                          -}
latFix :: Lattice a => (a -> a) -> a
latFix f = firstRepeat $ iterate f bottom
firstRepeat :: Eq a => [a] -> a
firstRepeat (x:y:rest) = if x==y then x else firstRepeat (y:rest)


-- ==============================================
-- 4. A HIGHER-ORDER HUNDLEY-MILNER TYPE SYSTEMS.
-- ==============================================

{-  HM allows us to generalize wrt types, but not wrt type-constructors.
    For instance, we may wish to define a function
    size :: forall t. forall a. t(a) -> Int
    that could be used to give some measure of the size of an object (t a) for any type
    constructor t, and any type a (for instance, we might expect that length would be a
    special case of size, using the list constructor in place of the varaible t).     -}
{-  This is a weak form of "higher-order polymorphism".
    We may be concerned that this generalization of HM will present technical difficulties.
    Standard type-inference requires a "unification" algorithm to determine when two types
    are equal. In the higher-order case, we need to be able to compare type constructors,
    which might seem to imply a need for higher-order unification, which is known to be
    undecidable. In fact, there is an effective type-inference algorithm, based on a
    (decidable) first-order kinded unification process. This is possible since the language
    of constructors is built up from constants and application. In particular, there are no
    abstractions. See Hindley, "The principal type system of an object in combinatorial
    logic".
    This has been used as an integral part of the system of "constructor classes" (see Jones,
    "A system of constructor classes: overloading and implicit higher-order polymorphism.")
    Our goal is to emphasize the fact that this higher-order extension is independent of any
    use of overloading.                                                                    -}
{-  The extension is based on the use of a "kind" system...
    K ::= *            -- monotypes
       |  k1 -> k2     -- function kinds
    Kinds identify particular families of type constructors in the same way that types describe
    collections of values. The kind * represents all monotypes, ie. types with nullary type
    constructors, while the kind k1->k2 describes types whose constructors take something of
    kind k1 and return soomething of kind k2.
    For each kind K, we have a collection of constructors C(K) (including constructor variables
    a(K)) of kind K given by...
    C(K) ::= X(K)               -- constants
          |  a(K)               -- variables
          |  C(K') ->K  C(K')   -- applications
    This corresponds quite closely to the way most type expressions in Haskell are already
    written. eg. (List a) is an application of the constructor constant List to the constructor
    variable a. Additionally, each constructor constant has a corresponding kind. For example..
    Int, Float, ()        ::  *
    List, BinTree         ::  * -> *
    (->), (,), LabTree    ::  * -> * -> *
    Now, the take of checking that a given type expression is well-formed can be reformulated
    as the task of checking that a given type constructor has kind *.
    Kinding in practice is implicit: resolved by a process of kind inference, without any need
    for programmer-supplied kind annotations.                                                -}
-- note: the implementation of this form of polymorphism is quite straightforward

--  Applications.
{-  We find that, on its own, higher-order polymorphism is often too general to be useful.
    For example, in the case of the size function above, we would need a definition that will
    work for *any* type constructor t, and *any* type a. The only possibilities are functions
    \x.n for constant n, and those can be treated as having the more general type 
    note: If we add extra constants to the type system, with higher-order polymorphic types,
          we make the type system much more powerful. We are assuming here that we have only
          the regular constants included in HM.                                            -}
{-  forall a. a -> Int, without any need for higher-order polymorphism.
    However, higher-order types are still useful, especially, for defining new datatypes using
    a mixture of types and type constructors as parameters.                                  -}

-- Examples.

-- These examples are discussed in detail in the next section...
{-  the following 3 types provide a framework for constructing recursive datatypes
    and corresponding recursion schemes.
Mu   :: (* -> *) -> *
data Mu f = In (f (Mu f))
NatF :: * -> *
data NatF s = Zero | Succ s
Nat  :: *
type Nat = Mu NatF
-}

{-   parametrized state monad.
StateT :: * -> (* -> *) -> * -> *
-}
data StateT s m a = STM (s -> m (a,s))


-- =======================
-- 5. CONSTRUCTOR CLASSES.
-- =======================

{-  Type classes and higher-order polymorphism are independent extensions to HM, but when
    combined produce a system of greatly enhanced expressive power.
    We previously used classes to represent sets of types, ie. constructors of kind *, but we
    can also use them to represent sets of constructors of any fixed kind K. These sets are
    called "constructor classes", and include the type classes of sect 3 as a special case. -}

-- Functors.
-- ---------

-- consider the map function...
map' :: (a -> b) -> ([a] -> [b])
{-  note that we consider map as a curried function here - it transforms a function between
    the base types a and b to a function from List a to List b.                           -}
map' f [] = []
map' f (x:xs) = f x : map' f xs
{-  map satisfies the following laws...
    1. map id = id
    2. map f . map g = map (g . g)                                                         -}
{-  We can define functions similar to map, satisfying the above laws, for many other datatypes.
    We call such function/type constructor combinations "functors". Thus List, together with 
    map, forms a functor.
    This is an obvious candidate for overloading, since the implementation of a particular
    variant of map (if it exists) is uniquely determined by the type constructor involved.  -}

-- Overloading map.
class Functor' f where
    fun :: (a -> b) -> (f a -> f b)
{-  fun must satisfy the "Functor Laws"...
    1. fun id = id
    2. fun f . fun g = fun (f . g)                                                           -}

-- we will use the following datatypes, all of which can be instantiated as functors...
data Id a = Id a
type List = []
data Maybe' a = Just' a | Nothing'
data Error a = Ok a | Fail String
data Writer' a = Result String a
type Reader' r = (->) r
{-  ((->) r) is a partial application of the constructor ->, thus (Read' a b) is another way
    of writing the type constructor (a -> b). Note that in this case, the "type" keyword is
    a misnomer, since (Read' r) has kind * -> * rather than just *.                       -}
-- The functor structures for these datatypes are given by the following instances...
instance Functor' Id where
    fun f (Id x) = Id (f x)
instance Functor' List where
    fun f [] = []
    fun f (x:xs) = f x : fun f xs
instance Functor' Maybe' where
    fun f (Just' x) = Just' (f x)
    fun f Nothing' = Nothing'
instance Functor' Error where
    fun f (Ok x) = Ok (f x)
    fun f (Fail s) = Fail s
instance Functor' Writer' where
    fun f (Result s x) = Result s (f x)
instance Functor' ((->) r) where
    fun f g = f . g
{-  Again, note the last example. We usually think of function composition and mapping over
    a list as two completely different things. However, we see here that they are both
    instances of the same concept: mapping over a functor.                                -}


-- Recursion Schemes: FP with Bananas and Lenses.
-- ----------------------------------------------

{-  Higher-order functions like map are useful because they abstract common patterns of
    computation. Map hides the underlying recursion and makes it easier to reason about
    programs.
    Another well-known HOF is foldr...                                                -}
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr f z xs)
-- again, there are variants for other datatypes. eg. a fold for RoseTrees...
foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT f (Node a xs) = f a (map (foldRT f) xs)
{-  This doesn't even have the same number of arguments as foldr - it seems unlikely that
    we should be able to use overloading to view these two functions as instances of the
    same concept. However, it is possible to do so if we adopt a more uniform way of
    defining recursive datatypes.                                                      -}
printRoseTree :: Show a => RoseTree a -> String
printRoseTree = foldRT (\x ts-> show x ++ concat ts)
flipRoseTree :: RoseTree a -> RoseTree a
flipRoseTree = foldRT (\x ts-> Node x (reverse ts))
{-  In category theory, where datatypes are constructed as fixed points of functors, the
    general version of a fold is a "catamorphism" (often notated with convex brackets
    (| phi |) and hence sometimes called "bananas") and there is a dual notion of
    "anamorphism" (notated with concave brackets )| psi |( and sometimes called "lenses").
    These ideas are explored in Meijer, et al, "Functional programming with bananas, lenses,
    envelopes and barbed wire."
    We will implement these ideas directly with constructor classes...                   -}

-- we start by defining a datatype for constructing fixed points of unary type constructors...
data Mu f = In (f (Mu f))
{-  Ideally, we would like to view the In constructor as an isomorphism of f (Mu f) and Mu f,
    with the inverse isomorphism given by...                                               -}
out :: Mu f -> f (Mu f)
out (In x) = x
{-  However, Haskell's lazy semantics treat In as a non-strict constructor, so these functions
    are not actually isomorphisms (there have been proposals to extend Haskell with mechanisms
    to allow such functions to be defined as actual isomorphisms.                           -}
{-  Now, using an appropriate function as a parameter, we use the Mu constructor to build
    recursive types...                                                                   -}
instance Show (f (Mu f)) => Show (Mu f) where
    show (In f) = show f

-- Recursive Types.
-- ----------------

-- Natural Numbers.
-- We define the datatype Nat of natural numbers as the fixed point of a functor NatF...
type Nat = Mu NatF
instance Show Nat where
    show (In Zero) = "zero"
    show (In (Succ (In Zero))) = "one"
    show (In (Succ (In (Succ (In Zero))))) = "two"
    show (In (Succ (In (Succ (In (Succ (In Zero))))))) = "three"
    show (In (Succ n)) = "Succ " ++ show n
data NatF s = Zero | Succ s deriving (Eq, Show, Read)
instance Functor NatF where
    fmap f Zero = Zero
    fmap f (Succ x) = Succ (f x)
-- For convenience, we define names for zero and for the successor function...
zero :: Nat
zero = In Zero
scc :: Nat -> Nat
scc x = In (Succ x)
-- example..
one = scc zero
two = scc one
three = scc two

-- Lists of Integers.
-- We define datatype IntListF as a functor, whose fixed point is recursive type IntList...
type IntList = Mu IntListF
data IntListF a = Nil | Cons Int a  deriving (Eq, Show)
instance Functor IntListF where
    fmap f Nil = Nil
    fmap f (Cons n x) = Cons n (f x)
instance Show IntList where
    show (In Nil) = "Nil"
    show (In (Cons n x)) = "(Cons " ++ show n ++ " " ++ show x ++ ")"
exRecIntListF :: IntListF (IntListF (IntListF (IntListF a)))  -- type of a 3-element IntListF
exRecIntListF = Cons 3 (Cons 5 (Cons 2 Nil))    -- "manual" recursive type using IntListF
exIntList :: IntList
exIntList = In (Cons 3 (In (Cons 5 (In (Cons 2 (In Nil)))))) -- proper recurs type Mu IntListF
nil = In Nil
cons x xs = In (Cons x xs)
-- the recursive type IntList can have members of any length.

-- Rose Trees.
type RTree a = Mu (RTreeF a)
data RTreeF a b = RNode a [b] deriving (Eq, Show)
instance Functor' (RTreeF a) where
    fun f (RNode x ts) = RNode x (map f ts)
exRTreeF = RNode 1 [RNode 2 [], RNode 3 [RNode 4 [], RNode 5 []], RNode 6 []]
exRTree = In (RNode 1 [In (RNode 2 [])
                      , In (RNode 3 [In (RNode 4 []), In (RNode 5 [])])
                      , In (RNode 6 [])])
node :: a -> [RTree a] -> RTree a
node x ys = In (RNode x ys)


{-  The general definitions of catamorphisms and anamrophisms can be expressed directly in
    this framework. We write "cata phi" for (|phi|) and "ana psi" for )|psi|(.           -}
cata :: Functor f => (f a -> a) -> Mu f -> a
cata phi = phi . fmap (cata phi) . out
ana :: Functor f => (a -> f a) -> a -> Mu f
ana psi = In . fmap (ana psi) .psi

-- example: definitions for arithmetic  on natural numbers
-- catamorphisms...
addNat :: Nat -> Nat -> Nat
addNat n m = cata (\fa -> case fa of
                            Zero -> m
                            Succ x -> scc x) n
mulNat :: Nat -> Nat -> Nat
mulNat n m = cata (\fa -> case fa of
                            Zero -> zero
                            Succ x -> addNat m x) n
expNat :: Nat -> Nat -> Nat
expNat n m = cata (\fa -> case fa of
                            Zero -> one
                            Succ x -> mulNat n x) m
-- catamorphism to calculate the length of an IntList...
len :: IntList -> Nat
len = cata (\fa -> case fa of
                     Nil -> zero
                     Cons n x -> scc x)
-- catamorphism to append 2 lists...
append :: IntList -> IntList -> IntList
append xs ys = cata (\fa -> case fa of
                             Nil -> ys
                             Cons z zs -> cons z zs) xs
-- anamorphism to construct an infinite list of natural numbers...
intsFrom :: Int -> IntList
intsFrom = ana (\n -> Cons n (n+1))


-- Monads.
-- -------
{-  Based on work by Moggi ("Computational lambda calculus and monads") and
    Spivey ("A functional theory of exceptions"), Wadler proposed the use of monads in FP,
    especially to model impure features in a purely functional language.
    One useful way to think of a monad is as a representation of computation. A monad (m a)
    is a computation resulting in a value of type a. The choice of monad reflects the
    particular PL features available for use in the computation, eg. state, exceptions, IO.
    Every monad provides at least two ops: a way to return values and a way to combine two
    computations.                                                                        -}
class Functor m => Monad' m where
    result :: a -> m a
    bind :: m a -> (a -> m b) -> m b
-- Note that these ops are required to obey the "monad laws", not reflected in the class defn
{-  One use of monads is to model progs using internal state. Such a prog can be represented
    by "state transformers", ie. functions ::s->(a,s) mapping an initial state to a result
    value paired with the final state.
    With constructor classes, we can represent state transformers with the datatype...    -}
data State s a = ST (s -> (a,s))
instance Functor (State s) where
    fmap f (ST st) = ST (\s -> let (x,s') = st s
                               in (f x, s'))
instance Monad (State s) where
    return x = ST (\s -> (x,s))
    (ST st) >>= f = ST (\s -> let (x,s1) = st s
                                  ST f' = f x
                                  (y,s2) = f' s1
                              in (y,s2))

-- many functors can also be given a natural monadic structure...

-- the identity monad provides a trivial base case for monad transformers (see below)..
instance Monad Id where
    return = Id
    Id x >>= f  =  f x
-- the list monad is useful for describing computations which may produce zero or more results
instance Monad' List where
    result x = [x]
    (x:xs) `bind` f  =  f x ++ (xs `bind` f)
-- the Maybe monad can model computations which either produce a result or raise an exception
instance Monad' Maybe where
    result x = Just x
    (Just x) `bind` f = f x
    Nothing `bind` f = Nothing
-- the Error monad is like Maybe, but includes a string error message on failure...
instance Monad Error where
    return = Ok
    Ok x >>= f = f x
    Fail msg >>= f = Fail msg
-- the Writer monad allows a program to produce both an output string and a return value
{-  Note: A serious implementation of Writer, like the one in the Haskell libs, would use
    functions of the form ShowS :: String -> String as the output component in place of the
    simple Strings used here. This is a well-known technique used to avoid the worst-case
    quadratic behaviour of nested calls to append (++).                                 -}
instance Monad Writer' where
    return x = Result "" x
    Result s x >>= f  =  let Result s' y = f x
                         in Result (s++s') y
-- A Reader monad lets a computation access values held in some enclosing environment...
{-
instance Monad ((->) r) where
    return x = \r -> x
    rx >>= f  =  \r -> let x' = rx r
                       in (f x') r
-}
-- note: these 2 functions are just the K and S combinators of combinatory logic


-- Operations on Monads.
-- ---------------------
{-  From a user's pov, the most interesting properties of a monad are not the standard return
    and bind ops, but the additional ops it supports, eg to deal with state or IO.
    Instead of just running through the list of monads above, we'll use the constructor class
    mechanism to define different families of monads, each of which supports a different set
    of simple primitive ops. This will be of benefit later, when we wish to consider monads
    which are simultaneously members of multiple classes and hence support a combination of
    primitive features. This approach has proven very flexible.                           -}

-- We'll use the following classes of monad...

-- State Monads: there is a way to access and update the state.
{-  we'll represent these 2 features by a single "update" op that applies a supplied function
    to the state and returns the old state as its result.                                   -}
class Monad m => StateMonad m s where
    update :: (s -> s) -> m s
instance StateMonad (State s) s where
    update f = ST (\s -> (s, f s))
-- a state monad can be used to maintain an integer counter...
incr :: StateMonad m Int => m Int
incr = update (+1)
