-- ==================================================
-- CIRCULAR PROGRAMS AND SELF-REFERENTIAL STRUCTURES.
--                 Lloyd Alison
-- ==================================================

{-  Purpose: to show that circular programs...
    1. can be used to create common data structures like doubly-linked lists,
       threaded trees and queues;
    2. are often more space-efficient than a conventional program, as they
       avoid the creation of temporary data structures that must be GC'd later;
    3. can often be tranlated into an imperative language if necessary.       -}

{-  A circular program involves a recursive or self-referential expression for a
    data structure.
let ds = f(ds)
in ds
    where ds is a data structure (not a function).                             -}

{-  Circular programs require lazy evaluation.                                 -}

-- --------------------
-- FUNCTIONAL EXAMPLES.
-- --------------------

-- Circular Lists.
-- ---------------

ones = let ones = 1 : ones
       in ones
-- circular list [f x, f (g x), f(g (g x)), ..., f(g(n) x)]
-- where n is the first integer for which pred(g(n) x) is true.
circ f g pred x = c
    where c = build x
          build y = (f y) : (if pred y then c else build (g y))
-- we could eliminate c from this definition...
noncirc f g pred x = build x
    where build y = (f y) : (if pred y then build x else build (g y))
{- but although it produces the correct value, this is no longer implemented as
   as circular list. Rather, it unfolds the infinite repeating list
    [f x, f (g x), f(g (g x)), ..., f(g(n) x), f x, ...]
   which is a much more wasteful way of representing the value.
   Hughes describes a way to memoize a program like noncirc to produce a circular
   data structure.
   see Hughes (1985), Lazy Memo-functions.                                      -}


-- Doubly-linked Lists.
-- --------------------

data DLList a = DLLNil | DLLNode (DLList a) a (DLList a)
double f g pred x = build DLLNil x
    where build prev y = if pred y then DLLNil else d
              where d = DLLNode prev (f y) (build d (g y))
{- note: I can derive instances of Eq, Show, etc, but they won't function properly.
   eg. show attempts to represent a dl-list [DLLNil 1,2,3] in the form DLLNode prev val next
   However, next is represented as DLLNode prev' val' next', where prev' is the original node
   value we were constructing. Thus, we end up with an infinitely nested representation
   DLLNode DLLNil 1 (DLLNode DLLNil 1 (DLLNode DLLNil 1 (.... which can never complete.
   TODO: write a Show (and perhaps Eq) instance for DLList.                           -}
{-  note: d is local to the build proc since the predecessor of a node was created by the
    preceding call to build. In the circular lists above, c was global to the build proc since
    the start point remains the same throughout the recursive calls.
    Since d is local to build, it is more difficult to remove than c, but it can be done using
    a fixed point operator. Again, this would remove the circularity of the data structure.  -}


-- Threaded Trees.
-- ---------------

data ThreadedTree a = TTNil
                    | Thread (ThreadedTree a)
                    | TTFork (ThreadedTree a) a (ThreadedTree a)
                    deriving (Show, Eq)
thread l = build True TTNil l
    where build isleft succ l =
              if null l
              then if isleft then TTNil else Thread succ
              else t
                  where t = TTFork
                            (build True t (filter (< head l) l))
                            (head l)
                            (build False succ (filter (> head l) l))
-- TODO: RETURN TO THIS LATER.


-- Breadth-First Traversal.
-- ------------------------

{-  We now deal with examples which use expressions which are recursive or circular,
    but where the values created are not: thus, no circular data structures are created -}
{-  Prefix, infix and postfix traversals of a tree are easily programmed in a FPL.
    However, breadth-first traversal is much harder. The usual imperative approach uses a
    queue and destructive assignment: an element is removed from the start of the queue and
    its children added to the end.
    However, we can write a circular program where elements are removed from the front of
    the queue while the end id still being computed.                                    -}
data Tree a = Empty
            | Fork (Tree a) a (Tree a)
            deriving (Eq, Show)
bfs :: Tree a -> [Tree a]
bfs t = r
    where r = case t of
                Empty -> []
                (Fork left elt right) -> t : (bf r 1)
          bf :: [Tree a] -> Integer -> [Tree a]
          bf q n = 
              if n==0
              then []    -- q is used up
              else let root = head q
                       rest = bf (tail q)
                   in case root of
                        Fork Empty _ Empty -> rest (n-1)
                        Fork left  _ Empty -> left : (rest n)
                        Fork Empty _ right -> right : (rest n)
                        Fork left  _ right -> left : right : (rest (n+1))
{-  bfs t returns a list of nodes in breadth-first order. For a nonempty tree, the first
    node is the root t itself; the remaining elements are given by bf t 1.
    bf absorbs a queue q with n known elements, while computing a result queue which is in
    fact, identified with q itself. bf places nonempty children in the "result queue" and
    adjusts its length parameter (for the next recursive call) accordingly. Each call to bf
    uses one element from q and adds 0,1 or 2 elements. rest is a function to build the
    result for the rest of the input queue after the current node.
    Note that bfs can traverse even infinite trees.                                       -}
bfs_elts tree = concat $ map elt $ bfs tree
    where elt Empty = []
          elt (Fork _ x _) = [x]


-- Unique (Nub).
-- -------------

{- consider the function unique, which takes a list and removes duplicates, maintaining order
   of first occurrence.                                                                     -}

-- we can easily write a version which preserves order of *last* occurrence...
uniqueL [] = []
uniqueL (x:xs)  = if x `elem` xs
                  then uniqueL xs
                  else x : (uniqueL xs)
-- so the original problem is solved by...
uniqueF :: Eq a => [a] -> [a]
uniqueF = reverse . uniqueL . reverse
-- however this creates garbage in the form of 2 intermediate lists.
-- another inefficient solution...
uniqueF2 [] = []
uniqueF2 (x:xs) = x : (filter (/= x) xs)
{- ...here, each call to filter creates a temp list, so the func makes O(|ls|) temp lists
   and uses O(|ls|^2|) space.                                                           -}
{-  An imperative programmer might take the following approach: build a list r element by
    element. For element of input list l, we check if it is already in r - if not, we add it -}
-- We can do this functionally: a circular program can examine the result list while building it
unique ls = r
    where r = u ls 0
          u [] _ = []
          u (x:xs) n = if member x r n
                       then u xs n
                       else x : (u xs (n+1))
          member e l n = if n==0    -- attempting to pattern-match on l will ruin circularity
                              then False
                              else if e == head l
                                   then True
                                   else member e (tail l) (n-1)
{-  r is the self-referential data structure that function u both creates and uses at the same
    time.
    member is a variation of elem with a parameter indicating the length of the list so far
    constructed.
    While r is being built its end is unknown: it terminates in a "recipe": hence, member
    cannot use null(l) (or pattern-match on l) to detect the current end-point of the search
    list; instead (as in bfs), we add an integer parameter n which keeps track of the length
    of the known part of r - it prevents member from "forcing" the recipe and causing an 
    infinite loop.
    Note: the shape of a list is given by a single integer, but the shape of a tree is more
    expensive to represent: a circular prog that computes a tree where computation depends on
    the shape of the part already evaluated, is unlikely to be so effiecient.
    Note: uniqueF2 and unique will both work on infinite lists, but only unique creates no 
    temp lists and runs in space linear to the amount of output.                         -}


-- Primes.
-- -------

-- Consider an implementation of the Sieve of Eratosthenes. A typical noncircular coding is...
sieve = let lsieve ls@(x:xs) = x : (lsieve (filter (\n-> n`mod`x /= 0) ls))
        in lsieve [2..]
{-  This creates many intermediate lists: [2..] and various sublists containing fewer and fewer
    composites, each from a successive call to filter.                                       -}
{-  In imperative programming, there are 2 main families of primes programs -
    1. Sieve: finds successive primes and removes all multiples of them from the set of numbers
    2. A set of successive primes is maintained. There is a loop over new candidates and each is
       tested for primality against the members of the list, after which the successful
       candidate may be added to the list. The filtering of each candidate becomes the inner
       operation. This process may be coded as follows...                                  -}
primLs = let pr = 2 : (filter (nomult pr) [3,5..])
         in pr
             where nomult (x:xs) n
                       | x^2 > n       = True
                       | n`mod`x == 0  = False
                       | otherwise     = nomult xs n
{-  primLs is self-referential. It starts with 2 and a sublist of [3,5..] follows.
    The predicate nomult tests candidates against primes already computed.
    Note: when a new number is tested, we only have to test against primes not exceeding
    its square root, and we know that all such primes are already calculated. Hence, there
    is no need to pass the number of known primes to nomult (the way we did when implementing
    bfs and unique).                                                                        -}
-- Note that the expression (nomult pr) is precisely the predicate isprime
-- We can thus rewrite this program...
primes = let pr = 2 : (filter isprime [3,5..]) in pr
isprime = nomult primes
    where nomult (x:xs) n | x^2 > n     = True
                          | n`mod`x==0  = False
                          | otherwise   = nomult xs n
-- primes and isprime form a mutually-recursive data structure and function pair.
-- alternaltively, substituting primes into isprime gives...
isprime' = nomult (2 : (filter isprime [3,5..]))
    where nomult (x:xs) n | x^2 > n     = True
                          | n`mod`x==0  = False
                          | otherwise   = nomult xs n
{-  These 3 programs are all equally effective (although the second has the advantage that one
    can refer to both prime and isprime). No circular data structure is created in any of thes
    programs.
    Note: isprime runs faster the second time it is used on a number of a given order of
    magnitude since the primes it needs have already been calculated.                  -}


-- ---------------------
-- IMPERATIVE LANGUAGES.
-- ---------------------

{-  While some circular programs are hard to implement in an imperative languages, some can be
    coded quite easily.


-- ------------
-- CONCLUSIONS.
-- ------------

{-  A circular prog uses a recursive expression for a data structure. In cases where evaluation
    of the expression incorporates the data structure directly, the result is a circular data
    structure. Circular programming allows classic data structures like circular and doubly-
    linked lists, threaded trees and queues, to be used in FP. As long as the structures are
    subject to the single-assignment rule, reference variables and assignment are not required.
    Circular programs are often more spqce-efficient than alternatives, since they can avoid the
    creation of intermediate data structures.
    Circular programming requires a lazy functional language. Note that GC based purely on
    reference counting can't collect circular structures, but mark-scan, copying and hybrid
    collectors can.
    Circular programs can often be translated into imperative languages.                   -}
