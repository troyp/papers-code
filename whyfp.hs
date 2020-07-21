{-# LANGUAGE FlexibleInstances #-}


-- ****************************
-- *                          *
-- *  "WHY FP?" by J. Hughes  *
-- *                          *
-- ****************************

-- ===========================
-- Glueing functions together.
-- ===========================
import Data.List

data Listof x = Nil | Cons x (Listof x)
                deriving Eq
instance (Show x) => Show (Listof x) where
    show Nil = "[]"
    show (Cons a b) = "[" ++ (show a) ++ (showContents b) ++ "]"
        where
          showContents :: Show x => (Listof x) -> String
          showContents Nil = ""
          showContents (Cons a b) = ", " ++ (show a) ++
                                    (showContents b)
convertList :: [a] -> Listof a
convertList = foldr Cons Nil
lsum :: (Listof Integer) -> Integer
lsum Nil = 0
lsum (Cons num list) = num + lsum list

reduce f x Nil = x
reduce f x (Cons a l) = f a (reduce f x l)
lsum2 = reduce (+) 0
product = reduce (*) 1
anyTrue = reduce (||) False
allTrue = reduce (&&) True
append a b = reduce Cons b a

doubleAll1 = reduce doubleAndCons Nil
    where doubleAndCons num list = Cons (2*num) list
doubleAll2 = reduce (Cons . (*2)) Nil

map' f = reduce (Cons . f) Nil
doubleAll = map' (*2)

sumMatrix = lsum . (map' lsum)

data Treeof x = Node' x (Listof (Treeof x))
                deriving (Eq, Show)
example_tree = Node' 1 (Cons 
                       (Node' 2 Nil)
                       (Cons
                        (Node' 3 (Cons (Node' 4 Nil) Nil))
                        Nil))
{-  recall that reduce has 2 args before the list: one that replaces Cons and
    one that replaces Nil. Since Treeof is built from Node', Cons and Nil, our
    reduceTree function will need an arg to replace each of these.    -}
reduceTree f g a (Node' label subtrees) =
    f label (reduceTree' f g a subtrees)
reduceTree' f g a (Cons subtree rest) =
    g (reduceTree f g a subtree) (reduceTree' f g a rest)
reduceTree' f g a Nil = a
sumTree = reduceTree (+) (+) 0
labels' = reduceTree Cons append Nil
mapTree f = reduceTree (Node' . f) Cons Nil
{-  note: I think a more general reduceTree would have 4 extra args, not 3.
    There would be an arg to replace Nil in (Node' label Nil); and a seperate
    arg to replace Nil in (Cons subtree Nil) in reduceTree'. The form given
    here makes it difficult to use since the same "identity" must be used for
    the "horizontal" operation and "vertical" operation. Consider a function
    to add sub-branches and multiply the result with their parents node's value,
    recursively. reduceTree (*) (+) 0 doesn't work - it always gives 0.
    But reduceTree (*) (+) 1 won't work either: it adds an extra 1 to each
    "horizontal addition" due to the Nil at the end of the list    -}
redTree f f_init _ _ (Node' label Nil) = f label f_init
redTree f f_init g g_init (Node' label subtrees) =
    f label (redTree' f f_init g g_init subtrees)
redTree' _ _ _ g_init Nil = g_init
redTree' f f_init g g_init (Cons subtree rest) =
    g (redTree f f_init g g_init subtree) (redTree' f f_init g g_init rest)


-- ==========================
-- Glueing programs together.
-- ==========================

-- note: I'm reverting to builtin Haskell lists here.

{-  Recall that a functional program is just a function form its input to its
    output. Thus, if we widh to chain together two functional programs, we can
    simply use function composition. If f takes input and produces output, we
    can pipe that input into another program g by using...
    (g . f) input    -- (g . f) is the composite program.
    In a lazy language, f does not need to do all its work before g starts.
    f and g are run in synchronisation. f is only started when g tries to read
    some input and is suspended once g obtains it. If g tries to read more
    input, f is resumed long enough to provide it. If g terminates, f is
    automatically aborted, regardless of whether it is finished. Thus, f could
    even be a nonterminating program producing infinite output.
    * This allows termination conditions to be seperated from loop bodies -
      one of the most powerful modularisations in functional programming.
    We can modularise a program as a (potentially infinite) generator which
    generates possible answers and a selector which cooses the appropriate one
    -}


-- Newton-Raphson square-root approximation.
-- -----------------------------------------
{-  approximates a root of N by starting with an initial approximation a(0) and
    improving it using the rule
    a(n+1) = ( a(n) + N/a(n) )/2
    The approximations rapidly converge on sqrt(N). A sqrt program usually
    takes a tolerance(eps) and stop when two successive approximations differ
    by less than eps.                -}
-- we will write a modularised version (and then reuse some parts)
next_nr n x = (x + n/x)/2    -- next-approximation function.
-- we'd like a list of successive approx's [a0, f a0, f(f a0), f(f(f a0),..]
frepeat f a = a : frepeat f (f a)
newton_raphson = frepeat . next_nr    -- given N, a0, generates the approx. list
-- we need a function that checks if the next 2 entries are within eps
within eps (a:b:rest) | abs(a-b) <= eps  = b
                      | otherwise        = within eps (b:rest)
-- composing a sqrt function from initial guess a0 with tolerance eps...
sqrt_nr1 a0 eps n = within eps (frepeat (next_nr n) a0)
{-  testing absolute tolerances can be problematic for very small numbers (where
    the difference is small to start with) and very large ones (where rounding
    errors may be greater than the tolerance). A better method is to use
    relative tolerances - ie. test the ratio of successive spproximations  -}
withinRel eps (a:b:rest)  | abs(a-b) <= eps * abs b  = b
                          | otherwise                = withinRel eps (b:rest)
-- Now, we only need to replace within by withinRel in our definition...
sqrt_nr a0 eps n = withinRel eps (frepeat (next_nr n) a0)


-- Numerical Differentiation.
-- --------------------------
easydiff f x h = ( f (x+h) - f x )/h
{-  To get the most accurate estimate of the derivative, we want h very small.
    However, if h is too small, rounding errors will swamp the result! One way
    to approach this dilemma is to generate a series of approximations with
    decreasing h, then use 'within' to choose the first one that is sufficiently
    precise.                -}
diffSeq h0 f x = map (easydiff f x) (frepeat (/2) h0)
{-  The derivative at any point can be computed by
    within eps (diffSeq h0 f x)
    However, this sequence converges fairly slowly. We can do better...
    each term in the sequence can be written as sqrt(n) + error(h)
    It can be shown that the error term is roughly proportional to a power of h.
    let h=2*h' for term i, so h=h' for term i+1
    So a(i) = sqrt(n) + k*((2*h')**n)
            = sqrt(n) + k*(2**n)*(h'**n)
    a(i+1)  = sqrt(n) + k*(h'**n)
    Solving... sqrt(n) = ( a(i+1)*(2**n) - a(i) ) / 2**n-1
    This is still only an approximation (since the error is only roughly
    proportional to a power of h), but a much better one.
    We can apply this transformation to each successive pair of our original
    approximation sequence.                -}
elimError n (a:b:rest) = (b*(2**n) - a)/(2**n - 1) : elimError n (b:rest)
{-  Before we can use this to improve our estimates, we need to know (or at
    least approximate) the value of n. This is difficult to predict in general,
    but easy to measure. It can be shown to be estimated by the following... -}
errOrder (a:b:c:rest) = (rnd . log2) ( (a-c)/(b-c) - 1 )
log2 :: (Floating t) => t -> t
log2 = (/(log 2)) . log
{-  note: for some reason, I'm having type issues with improveseq when
    errOrder is defined with "round". I need a Fractional rounding function
    so I'm defining a quick and dirty one...    -}
rnd x =
    let fracPart y | y < 1 = y
                   | otherwise = fracPart (y-1)
    in if fracPart x > 0.5 then x+1 - fracPart x else x - fracPart x
-- Now we can write our function to improve the sequence... t) => 
improveseq s = elimError (errOrder s) s
{- we can use this to efficiently compute a derivative to the required precision
   using...                -}
deriv1 h0 f x eps = within eps (improveseq (diffSeq h0 f x))
{-  improveseq works on sequences of approximations computed using a parameter
    which is halved between eazh approximation. Note that when applied to such
    a sequence, it's result is also such a sequence! Hence we can improve a
    sequence more than once. Each time, a different error term is eliminated
    and the resulting sequences converge faster and faster.    -}
-- we could compute a derivative very efficiently using...
deriv2 h0 f x eps = within eps (improveseq (improveseq (improveseq
                                                        (diffSeq h0 f x))))
-- (in Numerical Analysis terms, this is likely to be a "fourth-order" method)
{-  An even more sophisticated method would be to generate a sequence of
    more and more improved sequences of approximations, and then to construct
    a new sequence of approximations by taking the 2nd approximation from each
    sequence (it turns out the 2nd element is the best to take - it's more
    accurate than the 1st and doesn't require any extra work to compute).
    This algorithm uses better and better numerical methods as more and more
    approximations are computed. Despite being quite sophisticated, we can
    implement it quite easily.                -}
second (a:b:rest) = b    -- returns 2nd element of a list
superseq s = map second (frepeat improveseq s)    -- creates the "super" list
-- we could compute derivatives very efficiently indeed with this program...
deriv3 h0 f x eps = within eps (superseq (diffSeq h0 f x))


-- Numerical Integration.
-- ----------------------
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:s) (b:t) = (a,b) : zip' s t

easyintegrate f a b = (b-a) * (f a + f b)/2
{-  We can create a better approximation by splitting the interval in two,
    using easyintegrate on both and averaging the results. We can recursively
    generate a list of such approximations                -}
integrate1 f a b = easyintegrate f a b :
                  zipWith (+) (integrate1 f a mid) (integrate1 f mid b)
                  where mid = (a+b)/2
{-  This version is inefficient since it recomputes f (twice) and (f mid) each
    recursive call. A more efficient version that avoids recomputation... -}
integrate f a b =
    let integ f a b fa fb = (b-a)*(fa+fb)/2 :
                            zipWith (+) (integ f a m fa fm) (integ f m b fm fb)
            where m = (a+b)/2
                  fm = f m
    in integ f a b (f a) (f b)
{-  integrate computes a list of successively more precise approximations, just
    as diffSeq did. Thus, we can use within or withinRel on the list to obtain
    an integral to any required degree of precision.                -}
integrateToPrec f a b eps = within eps (integrate f a b)
{-  This sequence converges rather slowly. Note that our algorithm improves
    the approximation with each term by splitting the range (a,b) into twice as
    many intervals. Our approximation thus relies on interval length, which we
    can call h, and which is halved at each step. Thus integrate is a candidate
    for improvement with improveseq and superseq.                -}
piSeq = map (4*) (improveseq (integrate f 0 1))
    where f x = 1/(1 + x*x)
{-  The integral of 1/(1 + x^2) is arttan x, and tan(pi/4)=1, tan(0)=0
    So piSeq approximates pi                                -}
pi' = within 1e-16 piSeq


-- An example from AI.
-- -------------------
{-  The alpha-beta heuristic is an algorithm for estimating the strength of a
    game player's position by looking ahead to see how the game might develop,
    but avoiding unprofitable lines.
    We will let the game positions by represented by objects of type Position
    and that we have a function moves that indicates the possible next positions
    for a given position                                -}
data Piece = Nought | Cross | None
           deriving Eq
instance Show Piece where
    show Nought = "O"
    show Cross = "X"
    show None = "."
type Row = [Piece]
type Board = [Row]    -- I'll use Board for a 'structured' representation
type Position = [Piece]    -- Position is a linear representation of 9 pieces
emptyPos = replicate 9 None
-- we need a function to show the possible next moves
moves :: Position -> [Position]
{-  For any position, we must have the number of crosses equal to or one
    greater than the number of noughts. If the number is equal, it is crosses'
    move, if one greater, it is noughts' move next    -}
flatten = foldr (++) []    -- flattens a list of lists (non-recursively)
-- flatten will convert a Board into a Position
unflattenPos ls = [ [ls!!i, ls!!(i+1), ls!!(i+2)] | i<-[0,3,6] ]
{-  we can't write a general unflatten since information is lost, but we can
    write one for our 3x3 Board type                -}
showRow :: Row -> String
showRow = intersperse '|' . concatMap show
showBoard :: Board -> String
showBoard = unlines . intersperse "-----" . map showRow
printBoard = putStrLn . showBoard
printPos = printBoard . unflattenPos
countOf val = length . filter (\x->x==val)
indicesOf val ls = foldl (\z i->if (ls!!i)==val then z++[i] else z) [] ind
    where ind = [0..(length ls - 1)]
substitute :: Int -> a -> [a] -> [a]
-- return a list with a single sunstitution at a given index
substitute i val ls = (take i ls) ++ [val] ++ (drop (i+1) ls)
noughtMoves p = map (\i -> substitute i Nought p) (indicesOf None p)
crossMoves p  = map (\i -> substitute i Cross  p) (indicesOf None p)
moves p = if countOf Cross p > countOf Nought p
          then noughtMoves p
          else crossMoves p
-- does not remove symmetrical duplicates
{-  Note that we are assuming one can tell whose move it is from a given
    position. This is the case in Tic-Tac-Toe. In another game, like Chess,
    we may have to include this information explicitly as part of the Position
    datatype.                                                                -}
{-  Our first step is to construct a "game tree" - a tree where the nodes are
    labelled by Position and the children of a node are labelled with the
    possible next moves from that position. A game tree may be infinite, if the
    game can go on indefinitely.                -}
-- I'll make a version of the tree datatype that uses regular Haskell lists...
data Tree x = Node x [Tree x] 
              deriving (Eq, Show)
maptree :: (a -> b) -> (Tree a) -> (Tree b)
maptree f (Node a l) = (Node (f a) (map (maptree f) l))
{-  A game tree is built up by repeated application of "moves". A node is
    labelled with position p and it's children are labelled with positions
    given by (moves p), and so on recursively. We can represent this pattern
    with a HOF    -}
reptree f a = Node a (map (reptree f) (f a))
{-  Recursively builds a tree from top label a, where the node labelled a has
    children with labels given by (f a). Note that reptree is analagous to
    frepeat above for lists                             -}
gametree = reptree moves
{-  The alpha-beta algorithm looks ahead from a given position to see how the
    game will develop, but in order to do this it requires the ability to
    estimate the value of a position without looking ahead. This is called
    "static evaluation". Static evaluation is necessary at the limit of the
    look-ahead, and may be used earlier to guide the algorithm earlier. The
    simplest static evaluation function would return +1 when the computer has
    already won, -1 when it has lost, and 0 otherwise. In reality, static
    evaluation functions measure various aspects of a position that make it
    'look good', eg. in chess, it would involve material advantage and control
    of the centre.                                                -}
--static :: (Num a) => Position -> a
allrowsInd = [ [0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6] ]
allrows p = map (map (p!!)) allrowsInd
static p | any (\r->countOf Cross r == 3) (allrows p)  = 3
         | any (\r->countOf Nought r == 3) (allrows p) = -3
         | otherwise =lmax (map (countOf Cross) (allrows p)) -
                       lmax (map (countOf Nought) (allrows p))
                           where lmax = (foldl max 0) :: [Int] -> Int
-- MINIMAXING
{-  We can obtain a tree of static values by applying maptree static to the gametree.
    Given such a tree, what is the true value of the positions in it - the root node in
    particular? it's not the static value: this is only a rough guess. The value ascribed
    to a node should be determined recursively from its subnodes.
    We assume each player makes the best moves he can. When the computer moves, it will
    choose the move leading to the node with the maximum true value. When its opponent moves,
    he will make the move leading to the node with the lowest true value.
    Assuming they alternate turns, the true value of a node will be given by the function
    maximise when it's the computer's turn, and by minimise when its the opponent's turn -}
-- for nodes with no successor's, the true value is the static value
maximise1 (Node n []) = n
maximise1 (Node n sub) = maximum (map minimise1 sub)
minimise1 (Node n []) = n
minimise1 (Node n sub) = minimum (map maximise1 sub)
-- We may now be tempted to write a function to return the true value of a position...
evaluate1 = maximise1 . maptree static . gametree
{-  There are 2 problems with this -
    1. It doesn't work for infinite trees: the recursion never reaches a base case
    2. Even for finite trees, the gametree can be extremely large: it is practical to "prune"
       it: cut off all nodes beyond a certain distance from the root, so it only looks so far
       ahead.                                                                               -}
prune 0 (Node a x) = Node a []
prune n (Node a x) = Node a (map (prune (n-1)) x)
-- prune n forces us to use static values for nodes at depth n rather than recursing further...
evaluate2 = maximise1 . maptree static . prune 5 . gametree
{-  Note that lazy eval allows us to conveniently modularise in this way. Only those parts of
    data structures that are needed are evaluated, and they can be GC'd once no longer required.
    Hence, evaluate can be quite space efficient. If evaluation were strict, we could only gain
    this space-efficiency by folding all the functions in the chain together into one big one.
    Indeed, the modularized version may not work at all with strict eval, since the gametree
    may be infinite or astronomically large.                                               -}
-- ALPHA-BETA
{-  So far, we've just described minimaxing. The alpha-beta algorthm is based on the observation
    that we can often compute the value of maximise/minimise without looking at the whole 
    tree. Consider the tree (max (min 1 2) (min 0 ?)): it isn't necessary to know the value of
    ? to evaluate the tree.
    We can generalise this observation and build it into maximise and minimise.
    The first step is to split maximise into an application of maximum to a list of numbers
    (and to similarly decompose minimise - since the functions are symmetrical, we'll only
    discuss maximise)                                                                    -}
maximise :: Ord a => Tree a -> a
maximise = maximum . maximise'
minimise :: Ord a => Tree a -> a
minimise = minimum . minimise'
{-   Once we've decomposed the functions like this, maximise can use minimise' rather than
    minimise itself to discover which numbers minimise would take the minimum of. It may be
    able to discard some numbers without looking at them.                                 -}
maximise' (Node n []) = [n]
maximise' (Node n l)  = mapmin (map minimise' l)
    --where mapmin = map minimum    -- original defn; see below
minimise' (Node n []) = [n]
minimise' (Node n l)  = mapmax (map maximise' l)
    --where mapmax = map maximum    -- original defn; see below
{-  Since minimise' returns a list of numbers, the minimum of which is minimise, 
    (map minimise' l) returns a list of lists of numbers. maximise' should return a list of the
    minima of those lists. However, only the maximum of this list matters.
    Let's define a new version of mapmin which omits the minima of lists whose minimum doesn't
    matter...                                                                                -}
mapmin (l:ls) = (minimum l) : (omit (minimum l) ls)
    where omit pot [] = []    -- omit takes a "potential maximum" and omits minima less than it
          omit pot (l:ls) = if minleq l pot
                            then omit pot ls
                            else (minimum l) : (omit (minimum l) ls)
{-  minleq takes a list of numbers and a potential maximum and returns True if the minimum of
    the list is less-than-or-equal-to the potential maximum.
    To do this, it doesn't need to look at all of the list: if *any* element of the list is <=
    the potentail maximum, then the minimum of the list is sure to be, so all later elements
    are irrelevant, like the ? in our example above.                                       -}
-- defn of minleq from first priciples...
minleq1 [] pot = False
minleq1 (x:xs) pot = if x <= pot
                    then True
                    else minleq1 xs pot
-- we can more easily define minleq as...
minleq l pot = all (<= pot) l
-- symmetrical definitions for mapmax...
mapmax (l:ls) = (maximum l) : (omit (maximum l) ls)
    where omit pot [] = []    -- omit takes a "potential minimum" and omits maxima >= it
          omit pot (l:ls) = if maxgeq l pot
                            then omit pot ls
                            else (maximum l) : (omit (maximum l) ls)
maxgeq l pot = all (>= pot) l
-- Now it's simple to write a new evaluator...
evaluate3 = maximum . maximise' . maptree static . prune 8 . gametree
--  The simple optimisations in maximise' can have a dramatic effect on performance.
{-  There are other optimisations we can make. For instance, the alpha-beta algorithm we've
    implemented works best if the best moves are considered first, since if we've found a very
    good move, we don't need to consider worse moves, except to demonstrate that the oppenent
    has at least one reply to them.
    We might therefore like to sort the subtree lists at each node, highest value first when 
    it's the computer's move; lowest value first when it's not.                            -}
highfirst (Node n sub) = Node n (sortBy (flip compare) (map lowfirst sub))
lowfirst (Node n sub) = Node n (sort (map highfirst sub))
-- we'll need to be able to compare trees of Ints...
instance Ord (Tree Int) where
    compare (Node n _) (Node m _) = compare n m
-- Our further optimised evaluator...
evaluator = maximum . maximise' . highfirst . maptree static . prune 8 . gametree
{-  We may consider it sufficient to take only the 3 best moves for a player, in order to 
    restrict the search. To do so, we replace highfirst with (taketree 3 . highfirst), where -}
taketree n = redtree (nodett n) (:) []
    where nodett n label sub = Node label (take n sub)
-- version of redtree for our Tree type using native lists...
redtree f g a (Node label subtrees) =
    f label (redtree' f g a subtrees)
redtree' f g a (t:ts) =
    g (redtree f g a t) (redtree' f g a ts)
redtree' f g a [] = a
{-  Another improvement would be to refine the pruning. Our eval function looks ahead to a
    fixed depth, even if the position is very dynamic. It is usual to define certain "dynamic"
    positions and prevent the look-ahead to stop in one of these. eg. in Chess, we would not 
    end our look-ahead at a position where the queen was threatened.                        -}
-- given a predicate dynamic, we need add only a single clause to the defn of prune...
prune' 0 (Node a x) | dynamic a = Node 0 (map (prune 0) x)
                    | otherwise = Node a []
prune' n (Node a x) = Node a (map (prune (n-1)) x)
dynamic pos = False

{-  Making such changes is easy in a program as modular as this.
    As stated above - since the program's efficiency is critically dependent on an interaction
    between maximise', the last function in the chain, and gametree, the first - without lazy
    evaluation, it could only be written as a monolithic procedure. Such a program is hard to
    write, modify and understand.                                                           -}


-- Conclusion.
-- -----------

-- modularity is the key to successful and productive programming.
-- modularity means more than modules: scopes & seperate compilation are not enough
-- ability to decompose a problem directly depends on ability to glue solutions together.
-- a language must provide good glue!
-- FP glue..1. HOFs; 2. Lazy Evaluation. ...we can modularise in new ways!
-- This is why FP progs are shorter and easier
-- also target to aim for: if any part is messy/complicated, try to modularise and fix it
-- lazy eval perhaps most powerful glue FP possesses.