-- *****************
-- *               *
-- *  THE ZIPPER.  *
-- *               *
-- *****************

-- Consider the problem of representing a tree together with a subtree of interest.

-- The zipper is a simple data structure where tree editing is completely local,
-- the handle on the data being the subtree of interest, not the original root.
-- The tree is turned inside-out, with pointers from the root to the current node
-- being reversed in a *path structure*. The current *location* holds both the
-- downward current subtree and the upward path. All navigation and modification
-- primitives act on the location structure.
-- Moving up and down in the tree is analogous to opening and closing a zipper.

-- I don't find the "zipper" analogy very useful: the data structure is best seen
-- as a tree converted to local coordinates.

-- ==========================
-- The Zipper Data Structure.
-- ==========================

-- There are many variations. We first consider a version pertaining to trees with
-- variadic anonymous nodes and leaves injecting values from an unspecified "item"
-- type.

-- ===========================
-- Trees, paths and locations.
-- ===========================

data Tree t = Item t
            | Section { getSection :: [Tree t] }
              deriving (Eq, Show)
data Path t = Top
            | Node { getNode :: ([Tree t], Path t, [Tree t]) }
              deriving (Eq, Show)
-- A path is like a zipper, ripping the tree structure down to a certain level.
-- Node (l,p,r) contains a list l of elder siblings (starting with the youngest), a
-- father path and a list r of younger siblings (starting with the eldest).
-- A location in a tree addresses a subtree, together with its path...
data Location t = Loc { getLoc :: (Tree t, Path t) }
                  deriving (Eq, Show)

-- Note that a location does *not* correspond to an occurrence in the tree (as in
-- term rewriting or tree editors). Rather, it is a pointer to an arc linking a
-- designated subtree to its surrounding context.


-- example: parse tree of arithmetic expr   a*b + c*d   with String items ...
exTree1 = Section [ Section [ Item "a", Item "*", Item "b" ]
                  , Item "+"
                  , Section [ Item "c", Item "*", Item "d" ] ]
-- the location of the second * sign is...
exLoc1a = Loc ( Item "*"
              , Node ( [ Item "c" ]
                     , Node ( [ Item "+"
                              , Section [ Item "a", Item "*", Item "b" ] ]
                            , Top
                            , [] )
                     , [ Item "d" ] ) )

-- ===============================
-- Navigation primitives in trees.
-- ===============================

goLeft :: Location t -> Location t
goLeft (Loc (_, Top)) = error "left of top"
goLeft (Loc (x, Node ((l:left),up,right))) = Loc (l, Node (left,up,(x:right)))
goLeft (Loc (_, Node ([],up,right))) = error "left of first sibling"

goRight :: Location t -> Location t
goRight (Loc (_, Top)) = error "right of top"
goRight (Loc (x, Node (left,up,(r:right)))) = Loc (r, Node ((x:left),up,right))
goRight (Loc (_, Node (left,up,[]))) = error "right of last sibling"

goUp :: Location t -> Location t
goUp (Loc (_, Top)) = error "above top"
goUp (Loc (x, Node (left, Node (l,u,r), right))) = Loc (Section (left++(x:right)), Node (l,u,r))
goUp (Loc (x, Node (left,Top,right))) = Loc (Section (left++(x:right)), Top)

goDown :: Location t -> Location t
goDown (Loc ((Item _), _)) = error "below item"
goDown (Loc ((Section (x:xs)), path)) = Loc (x, Node ([], path, xs))
goDown _ = error "below empty"

-- goLeft, goRight and goDown are constant-time
-- goUp takes time linear in the "juniority" of the current term (ie. length left)

-- we can use these primitives to write a function for the nth son of the currnet term
nth :: Integral n => n -> Location t -> Location t
nth 1  = goDown
nth n  = if n < 1
            then error "nonpositive index"
            else goRight . nth (n-1)

-- ==================================
-- Changes, insertions and deletions.
-- ==================================

-- we can change the structure at the current location as a local operation
change :: Location t -> Tree t -> Location t
change (Loc (_,p)) t = Loc (t,p)

-- insertion to the left or right is natural and cheap...
insertRight :: Location t -> Tree t -> Location t
insertRight (Loc (_,Top)) _ = error "insertion right of top"
insertRight (Loc (t, Node (left,up,right))) r = Loc (t, Node (left,up,(r:right)))

insertLeft :: Location t -> Tree t -> Location t
insertLeft (Loc (_,Top)) _ = error "insertion left of top"
insertLeft (Loc (t, Node (left,up,right))) l = Loc (t, Node ((l:left),up,right))

-- insertDown inserts a son at the leftmost position under current, and moves to it...
insertDown :: Location t -> Tree t -> Location t
insertDown (Loc (Item _, _)) _ = error "insertion under item"
insertDown (Loc (Section sons, p)) t1 = Loc (t1, Node ([], p, sons))

-- delete primitive deletes tree at current position, then moves right if possible, otherwise
--   left, or up in the case of an empty list...
delete :: Location t -> Location t
delete (Loc (_,Top)) = error "delete top"
delete (Loc (_, Node (left,up,(r:right)))) = Loc (r, Node (left,up,right))
delete (Loc (_, Node ((l:left),up,[]))) = Loc (l, Node (left,up,[]))
delete (Loc (_, Node ([],up,[]))) = Loc (Section [], up)

-- This set of datatypes and operations should be sufficient to program the kernel
-- of a structure editor in an applicative yet efficient manner.


-- ********************************
-- *                              *
-- * Variations on the Basic Idea *
-- *                              *
-- ********************************

-- ======
-- Scars.
-- ======

 -- When an algorithm has frequent operations which necessitate going up and then
 -- down in the same location, it's a waste of time and space to close the sections
 -- in-between.
 -- It may be more efficient to leave "scars" in the structure, allowing direct
 -- access to the memoized visited positions.
 -- Thus, we replace nonempty sections with triples memoizing a tree and its
 -- siblings...

data MemoTree t = MItem t
                | Siblings { getSiblings :: ([MemoTree t], MemoTree t, [MemoTree t]) }
                  deriving (Eq, Show)
data MemoPath t = MTop
                | MNode { getMNode :: ([MemoTree t], MemoPath t, [MemoTree t]) }
                  deriving (Eq, Show)
data MemoLocation t = MLoc { getMLoc :: (MemoTree t, MemoPath t) }
                      deriving (Eq, Show)

-- simplified up and down primitives for these memoized structures...
goUpMemo :: MemoLocation t -> MemoLocation t
goUpMemo (MLoc (_,MTop)) = error "above top"
goUpMemo (MLoc (t, MNode (left,p',right))) = MLoc (Siblings (left,t,right), p')

goDownMemo :: MemoLocation t -> MemoLocation t
goDownMemo (MLoc (MItem _, _)) = error "below item"
goDownMemo (MLoc (Siblings (left, t', right), p)) = MLoc (t', MNode (left,p,right))

-- TODO: convert other primitives; make conversion procs...


-- ==================
-- First-Order Terms.
-- ==================

--  So far, our structures are untyped, with unlabelled tree nodes.
--  We have a kind of structured editor, like Lisp but oriented more towards
--  "splicing" operations than RPLACA/RPLACD.
--  If we want to implement a rree manipulation editor for ASTs, we need to
--  label out nodes with operator names.
--  If we use Items for this purpose, the usual Lisp encoding of first-order
--  terms would code F(T1...Tn) as the tree Section [Item(F), T1, ..., Tn]
--  Combinatory Logic suggests a dual solution where the comb-like structure
--  respects the applicative ordering - [Tn, ..., T1, Item(F)]
--  Neither of these solutions respects arity.

--  We won't pursue generic variations anymore, but consider adapting the idea to
--  a *specific* given signature of operators given with their arities, in such a
--  way that the tree edition maintains well-formedness with respect to the
--  arities of operations.

--  Basically, to each constructor F with arity n, we associate n path operators
--  Node(F,i).
--  Each has one path arg (upwards path) and (n-1) tree args (for its n-1
--  subling nodes). Node(F,i) is used when going down the ith subtree of an
--  F-term.

-- ==========================================
-- example: the structure for binary trees...
-- ==========================================

data BinaryTree t = Nil
                  | Cons (BinaryTree t, BinaryTree t)
                    deriving (Eq, Show)

data BinaryPath t = BinTop
                  | BinLeft (BinaryPath t, BinaryTree t)
                  | BinRight (BinaryTree t, BinaryPath t)
                    deriving (Eq, Show)

data BinaryLocation t = BinLoc (BinaryTree t, BinaryPath t)
                        deriving (Eq, Show)

changeBinTree :: BinaryLocation t -> BinaryTree t -> BinaryLocation t
changeBinTree (BinLoc (_,p)) t = BinLoc (t,p)

btgoBinLeft :: BinaryLocation t -> BinaryLocation t
btgoBinLeft (BinLoc (_,BinTop)) = error "left ot top"
btgoBinLeft (BinLoc (t, BinLeft (_,_))) = error "left of BinLeft"
btgoBinLeft (BinLoc (t, BinRight (left,father))) = BinLoc (left, BinLeft (father,t))

btgoRight :: BinaryLocation t -> BinaryLocation t
btgoRight (BinLoc (_,BinTop)) = error "right of top"
btgoRight (BinLoc (t, BinRight (_,_))) = error "right of BinRight"
btgoRight (BinLoc (t, BinLeft (father,right))) = BinLoc (right, BinRight(t,father))

btgoUp :: BinaryLocation t -> BinaryLocation t
btgoUp (BinLoc (_,BinTop)) = error "above top"
btgoUp (BinLoc (t, BinLeft (father,right))) = BinLoc (Cons (t,right), father)
btgoUp (BinLoc (t, BinRight (left,father))) = BinLoc (Cons (left,t), father)

btgoFirst :: BinaryLocation t -> BinaryLocation t
btgoFirst (BinLoc (Nil,_)) = error "first son of leaf"
btgoFirst (BinLoc (Cons (left,right), p)) = BinLoc (left, BinLeft (p,right))

btgoSecond :: BinaryLocation t -> BinaryLocation t
btgoSecond (BinLoc (Nil,_)) = error "second son of leaf"
btgoSecond (BinLoc (Cons (left,right), p)) = BinLoc (right, BinRight (left,p))
