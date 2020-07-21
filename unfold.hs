-- =============================
--  THE UNDERAPPRECIATED UNFOLD
--        Geraint Jones
-- =============================

-- 2. NOTATION.
-- ============
-- 2.1. Folds over lists.
-- ----------------------
-- We use 2 kinds of folds on lists: the normal fold-right..
foldr_ :: (a->b->b) -> b -> [a] -> b
foldr_ op e [] = e
foldr_ op e (a:x) = a `op` foldr_ op e x
-- ..and a restricted version for nonempty lists...
foldr1_ :: (a->a->a) -> [a] -> a
foldr1_ op l = foldr_ op (last l) (init l)
-- The two are related by the property that -
--   if e is a right-unit of op, then for nonempty x,  foldr op e x = foldr1 op x

{-  The normal fold enjoys a "universal property".
    This means essentially, that if the definition of foldr
        foldr op e (a:x) = a `op` foldr op e x
    is treated as an equation in the "unknown" (foldr op e), it has a unique (strict) solution.
    ie., for strict h,
        h = foldr op e
        <==>
        h [] = e  and  h (a:x) = a `op` h x

    A number of "promotion properties" follow...

    fold-map promotion: if is strict and  f (a `op` b) = f a `op2` f b
            then  f . foldr op e  =  foldr op2 (f e) . map f
                  f . foldr1 op   =  foldr1 op2 . map f

    fold-join promotion: if h = foldr op e . map f, where op is associative with identity e,
            then  h (xs ++ ys)  =  h xs `op` h ys

    fold-concat promotion: if h = foldr op e . map f, where op is associative with identity e,
            then  h . concat  =  foldr op e . map h
-}

-- 2.2. Unfolds over lists.
-- ------------------------
-- Unfolds are dual to folds.
-- The standard construction is...
unfold_a :: (b -> Either () (a,b)) -> b -> [a]
unfold_a pfg x = case pfg x of
                 Left ()     -> []
                 Right (a,y) -> a : unfold_a pfg y
-- however, we'll use the equivalent formulation...
unfold :: (b->Bool) -> (b->a) -> (b->b) -> b -> [a]
unfold p f g x
       | p x       = []
       | otherwise = f x : unfold p f g (g x)
-- alternative param names...
unfold' pred term next x
       | pred x       = []
       | otherwise    = term x : unfold' pred term next (next x)

{-  Unfolds also have the universal property: the above defn, viewed as an equation in
    (unfold p f g), has a unique solution. ie.
        h = unfold p f g
        <==>
        h x  =  if p x then [] else fx : h (g x)
-}

{-  Rather than the setting SET of total functions between sets, we'll work in the setting
    CPO of continuous functions between pointed complete partial orders, as advocated by
    Meijer et al ("Functional Programming with bananas, lenses, envelopes and barbed wire").
    This setting better matches the semantics of most functional languages.
    In particular, folds and unfolds cannot be composed in SET since the data structures
    produced by unfolds are different than the data structures consumed by folds (due to the
    treatment of infinite data structures). In CPO the data structures involved are the same,
    so folds can be composed with unfolds.
-}

-- 2.3. Trees.
-- -----------
-- We define a Rose Tree datatype...
data Tree a = Nd a [Tree a]  -- a tree consists of a root label and a list of subtrees
-- We define 2 deconstructors: root and kids...
root :: Tree a -> a
root (Nd a ts) = a
kids :: Tree a -> [Tree a]
kids (Nd a ts) = ts
-- It turns out simpler to perform most calculations on "forests" (ie. lists of trees)...
type Forest a = [Tree a]

exTree1 = Nd 1 [(Nd 2 [(Nd 5 []), (Nd 6 [])]),
                (Nd 3 []),
                (Nd 4 [Nd 7 []])]
exTree2 = Nd 3 [Nd 4 [], Nd 5 []]
exTree3 = Nd 5 []
exForest1 = [exTree1, exTree2, exTree3]

-- 2.4. Folds over trees.
-- ----------------------
{-  Note that the datatypes of Tree and Forest are mutually recursive: hence, folds over Trees
    and over Forests are also mutually recursive...
    These folds take 2 function args: the second combines a list of subtrees into a value;
    the first combines the root label with this value to produce an (intermediate) result.
-}
foldt :: (a->c->b) -> ([b]->c) -> Tree a -> b
foldt f g (Nd a ts) = f a (foldf f g ts)
foldf :: (a->c->b) -> ([b]->c) -> Forest a -> c
foldf f g ts = g (map (foldt f g) ts)

-- eg. sumt, sumf which sum a tree/forest of numbers...
sumt :: Num a => Tree a -> a
sumt = foldt (+) sum
sumf :: Num a => Forest a -> a
sumf = foldf (+) sum
-- (note: if sig not provided, moonomorphism restriction gives type Tree Integer -> integer)


-- 3. BREADTH-FIRST TRAVERSAL.
-- ===========================
{-  A "tree traversal" is a method which, given a tree, computes a list of the elements of that
    tree in some order. eg...
    Depth-First Traversals include: preorder, postorder, inorder (binary trees)
    preorder traversal: each parent appears before any of its children; siblings appear in
        left-to-right order.
    postorder traversal: each parent appears after all of its children; siblings appear in
        left-to-right order.
    inorder traversal (for binary trees): each parent appears after its left child and before
        its right child.
    Breadth-First Traversal: usually siblings appear in left-to-right order, then their
        children appear in the same order.
    level-order traversal: this is one, more declarative, way to characterize a breadth-first
        traversal; the tree is given as a list of lists, where each list contains the nodes at
        one "level", appearing in left-to-right order.

    Depth-first traversals are easy to implement in FP because they naturally follow the
    structure of the tree - ie. they can be expressed as folds over trees...
-}
preordert :: Tree a -> [a]
preordert = foldt (:) concat
ex_preordert = preordert exTree1    -- [1,2,5,6,3,4,7]

{-  OTOH, breadth-first traversal goes against the structure of the tree.
    It's not nearly as obvious how to implement BFT efficiently in a functional language.
    In particular, BFT is *not* a fold, since the traversal of a forest cannot be constructed
        from the traversals of the trees in that forest.
    In imperative languages, the standard implementation of BFT uses a queue, which is an
        awkward data structure in FP because it requires fast access to both ends of a list.
    DFT, OTOH, uses a stack, which "comes free" with a recursive program.
    It is possible to implement the queue-based algorithm efficiently in a pure FPL (see sect6).
    However, we would prefer a simpler - and more declarative - solution.
    This is acieved using a level-order traversal, which gives us a list of lists containing
        the elements at each level. To obtain the BFT, we simply concatenate the levels.
-}


-- 4. TRAVERSAL AS A FOLD.
-- =======================
{-  We present a characterization of level-order traversal as a fold over trees and forests...
levelt :: Tree a -> [[a]]
levelf :: Forest a -> [[a]]
-}
-- Given the level-order traversal of a tree or forest, the BFT is formed by concatenation...
bftt :: Tree a -> [a]
bftt = concat . levelt
bftf :: Forest a -> [a]
bftf = concat . levelf

{-  The level-order traversal of a forest is found by "gluing together" the LOTs of the trees.
    lzc ("long zip with concatenate") glues together two lists of lists in the appropriate way.
    (note: "long zip with" refers to zipWith; "long" because lzw returns a lists as long as its
    *longer* argument, whereas zipWith returns a list as long as its shorter argument)
-}
-- lzc: long zipWith compose - combines 2 lists of lists by concatenating corresponging elements
lzc :: [[a]] -> [[a]] -> [[a]]
lzc = lzw (++)
-- lzw: long zipWith
lzw :: (a->a->a) -> [a] -> [a] -> [a]
lzw op xs ys
    | null ys   = xs
    | null xs   = ys
    | otherwise = head xs `op` head ys : lzw op (tail xs) (tail ys)
-- note that (lzw op) is associative when op is

{-  Now, to define the LOT with a fold, we need 2 functions to pass to foldt and foldf:
    * a function to combine a root label with the traversal of a forest of child trees
    * a function to combine the traversals of a forest of child trees into a single traversal
-}
-- We now define the function glue to glue together the traversals of trees in a forest...
glue :: [[[a]]] -> [[a]]
glue = foldr lzc []
{-  The level-order traversal of a tree consists of its root "pushed" onto the traversal of the
    forest of its children, so we define...
-}
push :: a -> [[a]] -> [[a]]
push a xss = [a] : xss
-- push = (:) . (:[])    -- pointfree version.

-- We can now define levelt and levelf...
levelt_1 :: Tree a -> [[a]]
levelt_1 = foldt push glue
levelf_1 :: Forest a -> [[a]]
levelf_1 = foldf push glue

-- ----ASIDE----
-- lzw' = (uncurry . lzw) is an unfold...
lzw' :: (a->a->a) -> ([a],[a]) -> [a]
lzw' op = unfold p f g where
    p (xs,ys) = null xs && null ys
    --
    f (xs,[]) = head xs
    f ([],ys) = head ys
    f (xs,ys) = head xs `op` head ys
    --
    g (xs,ys) = (tail' xs, tail' ys)
    --
    tail' [] = []
    tail' zs = zs
{-  However, this uncurried version is inconvenient to use, since the standard defn of foldr
    requires a curried operator. Additionally, it is less efficient than the direct recursion,
    since it takes time proportional to the result length (= length oflonger arg), while the
    direct arg only traverses the shorter arg.
-}
-- ---END ASIDE---


-- 4.1. Traversal as a fold in linear time.
-- ----------------------------------------
{-  This characterization of LOT (and hence BFT) takes more than linear time.
    Consider the forest  ts = [ Nd 1 [t,u], Nd 2 [v,w] ], where t,u,v,w are trees.
    Unfolding the defns, we have...
    levelf ts = foldf push glue ts = glue (map (foldt push glue) ts)
              = foldr lzc [] (map (foldt push glue) ts) = foldr lzc [] (map levelt ts)
              = foldr lzc [] [ levelt (Nd 1 [t,u]), levelt (Nd 2 [v,w]) ]
              = lzc [levelt (Nd 1 [t,u]), lzc (levelt (Nd 2 [v,w])) []]
              = lzc ([1] : lzc (levelt t)
                               (lzc (levelt u) p[]))
                    (lzc ([2] : lzc (levelt v)
                                    (lzc (levelt w) []))
                         [])
    Note that (levelt t) and (levelt u) must be traversed once each to compute levelf [t,u],
    then again to compute levelf ts. In a complete binary tree of depth d, the LOTs of the
    deepest subtrees will be traversed (d-1) times. The whole algorithm takes time proportional
    to the size of the forest times its depth.
-}
-- We can remove this inefficiency by introducing an *accumulating parameter* into levelt/f...
-- We introduce auxillary functions levelt' and levelf'...
levelt'_1 :: Tree a -> [[a]] -> [[a]]
levelt'_1 t xss = lzc (levelt t) xss
levelf'_1 :: Forest a -> [[a]] -> [[a]]
levelf'_1 ts xss = lzc (levelf ts) xss
{-  Note that this is a generalisation, since..
    levelt t  = levelt' t []
    levelf ts = levelf' ts []
-}
{-  Now, for levelf', we have...
      levelf' [] xss   =   lzc (levelf []) xss   =   xss
    and..
      levelf' (t:ts) xss   =   lzc (levelf (t:ts)) xss
          =   lzc (lzc (levelt t) (levelf ts)) xss
          =   lzc (levelt t) (lzc (levelf ts) xss)    -- lzc is associative
          =   levelt' t (levelf' ts xss)    -- defn of levelt',levelf', applied in reverse

    For levelt', we must consider 2 cases...
      levelt' (Nd a ts) []   =   lzc (levelt (Nd a ts)) []
          =   levelt (Nd a ts)   =   [a] : levelf ts   =   [a] : levelf' ts []
      levelt' (Nd a ts) xss , where xss is non-empty
          =   lzc (levelt (Nd a ts)) xss
          =   lzc ([a] : levelf ts) xss
          =   (a : head xss) : lzc (levelf ts) (tail xss)    -- since xss is nonempty
          =   (a : head xss) : levelf' ts (tail xss)
    
    So, we can define...
-}
levelt' :: Tree a -> [[a]] -> [[a]]
levelt' (Nd a ts) xss = (a:ys) : (levelf' ts yss)
    where (ys, yss) | null xss  = ([], [])
                    | otherwise = (head xss, tail xss)
levelf' :: Forest a -> [[a]] -> [[a]]
levelf' ts xss = foldr levelt' xss ts
{-  which takes linear time (the efficient long zip lzw is necessary: if the unfold version lzw'
    is used, the program is still worst-case quadratic.
    Unfortunately, this efficient characterization of LOT is no longer a fold: the traversal of
    a forest is no longer constructed from the independant traversals of its trees; instead, the
    trees must be considered right-to-left, with the traversal of one used as a starting point
    for constructing the traversal of the next.
    This means we lose the benefits of HOFs: the code is more difficult to read, and is no
    longer suitable for parallelization since the accumulating parameter is single-threaded
    throughout the computation.
    We will consider the characterization of levelf as an *unfold*...
-}

-- ---ASIDE---
{-  It is possible to regain a characterization as a fold, while taking linear time, by
    abstracting from the accumulating parameter and constructing a function *between* lists of
    lists, in a "continuation-based" or "higher-order fold" style...
    [ Wand, "Continuation-Based program transformation strategies", 1980;
      Fegaras & Sheard, "Revisiting catamorphisms over datatypes with embedded functions", 1996 ]
-}
-- FIXME: defns not correct
--levelt'' :: Tree a -> [[a]] -> [[a]]
levelt'' = foldt f g
    where f a hss = (a:) : hss    -- takes a tree & a list of funcs; prepends func (a:) to list
          g = foldr (lzw (.)) []
--levelf'' ::  Forest a -> [[a]] -> [[a]]
levelf'' = foldf f g
    where f a hss = (a:) : hss
          g = foldr (lzw (.)) []
-- ---END ASIDE---


-- 5. TRAVERSAL AS AN UNFOLD.
-- ==========================
-- We wish to find p, f, g such that  levelf = unfold p f g
{-  Note that (levelf ts) is empty iff ts is empty, so
      level ts = []   <--->   null ts         This will be p.
    
Now, we consider nonempty forests only...

      head . levelf  =  head . foldr lzc [] . map levelt
                     =  head . foldr1 lzc . map levelt       (nonempty list)
    { fold-map promotion:  f . foldr1 op   =  foldr1 op2 . map f }
    { for nonempty xs,ys: head (lzw f xs ys) = f (head xs) (head ys) }
                     =  foldr1 (++) . map head . map levelt
    { head . levelt = (:[]) . root }
                     =  foldr1 (++) . map (:[]) . map root
    { for nonempty lists: foldr1 (++) . map (:[]) = id }
                     =  map root
    This will be f.
    
      tail . levelf  =  tail . foldr lzc [] . map levelt
                     =  tail . foldr1 lzc . map levelt       (nonempty list)
    { for nonempty xs,ys: tail (lzw f xs ys) = lzw f (tail xs) (tail ys)  + fold-map promotion }
                     =  foldr1 lzc . map tail . map levelt
    { tail . levelt = levelf . kids }
                     =  foldr1 lzc . map levelf . map kids
    { for nonempty lists, identity elem e: foldr op e = foldr1 op }
                     =  foldr lzc [] . map levelf . map kids
    { fold-concat promotion: if h = foldr op e . map f, where op is assoc with identity e,
        then  h . concat  =  foldr op e . map h
      levelf = foldr lzc [] . map levelt (see earlier) }
                     =  levelf . concat . map kids
    This will be (levelf . g)
    so we have... p = null
                  f = map root
                  g = concat . map kids
-}
levelf :: Forest a -> [[a]]
levelf = unfold null (map root) (concat . map kids)
-- We can define levelt in terms of levelf...
levelt :: Tree a -> [[a]]
levelt t = levelf [t]
{-  These unfold-based functions give us a simple linear-time traversal algorithm.
    We'll see that it's also amenable to manipulation: we'll use deforestation to remove the
      intermediate list of lists formed during the BFT.
-}
-- 5.1. Deforestation.
-- -------------------
{-  One of the benefits of expressing levelf as an unfold is that bftf is now a "hylomorphism",
    ie. an unfold followed by a fold.
    Hylomorphisms proceed in 2 stages: the first producing a data structure; the second consuming
      it. However, with lazy evaluation, the intermediate data structure need never be built as a
      whole: the 2 stages can operate concurrently, the producer producing each value as the
      consumer requires it.
    Even with lazy evaluation, it's still advantageous to merge the 2 stages into one, in order to
      reduce the amount of heap space turned over. This transformation is called "deforestation".
    Deforestation is now a standard technique, and can even be performed mechanically.
      see Onoue et al: "A calculational fusion system HYLO", 1997.
          Hu et al: "Deriving structural hylomorphisms from recursive defintions", 1996.

    We'll use deforestation on functions of the form...
      h = foldr op e . unfold p f g
    Consider the case where predicate p holds of the arg...
      h x = foldr op e [] = e
    Now, assuming p does not hold...
      h x = foldr op e (unfold p f g x)
          = foldr op e (f x : unfold p f g (g x))       { since p x is False }
          = f x `op` foldr op e (unfold p f g (g x))    { by defn of foldr }
          = f x `op` h (g x)
    So we have...
      h x  | p x       = e
           | otherwise = f x `op` h (g x)
    
    Applying this to bftf = concat . levelf...
      bftf = foldr (++) [] . unfold null (map root) (concat . map kids)
-}
bftf_dft ts 
    | null ts   = []
    | otherwise = map root ts ++ bftf_dft (concat (map kids ts))
-- the version generated by HYLO also deforests away the ++ and concat . map kids


-- 6. TRAVERSAL USING A QUEUE.
-- ===========================
{-  It turns out that the standard queue-based traversal algorithm arises from expressing bftf
    directly as an unfold, starting from the characterization of LOT as a fold.

    The calculation depends on the following property of lzw...
    THM: if op is associative, then...
      foldr op e (lzw op (x:xs) ys)  =  x `op` foldr op e (lzw op xs ys)
    Proof: [ by induction on xs ]
    Corollary: concat (lzc [xs1,xs2], [ys1,ys2,ys3]  =  concat [xs1++ys1, xs2++ys2, ys3]
                                                     =  (xs1++ys1) ++ (xs2++ys2) ++ ys3
                                                     =  xs1 ++ (ys1++xs2) ++ ys2 ++ ys3
                                                     =  xs1 ++ concat (lzc [ys1,ys2,ys3],[xs2])

    So, calculating bftf as an unfold from its fold defn ...
    bftf ts = []  <--->  null ts

    For a non-null forest, we have...
    bftf (Nd a us : ts)  =  (concat . levelf) (Nd a us : ts)
                         =  concat (foldr lzc [] (map levelt (Nd a us : ts)))
                         =  concat (lzc (levelt (Nd a us)) (foldr lzc [] (map levelt ts)))
                         =  concat (lzc (levelt (Nd a us)) (levelf ts))
                         =  concat (lzc ([a] : levelf us) (levelf ts))
                         =  [a] ++ concat (lzc (levelf ts) (levelf us))    { Corollary above }
    { fold-join promotion: if h = foldr op e . map f, where op is associative with identity e,
      then  h (xs ++ ys)  =  h xs `op` h ys    . Here, h=levelf, op=lzc, e=[] }
                         =  [a] ++ concat (levelf ts ++ us)
                         =  a : bftf (ts ++ us)

    So, we have...
-}
bftf_q :: Forest a -> [a]
bftf_q = unfold null f g
    where f (Nd a us : ts) = a
          g (Nd a us : ts) = ts ++ us

{-  Of course, bftf_q is not linear-time: appending the children us of the first tree to the end
    of the queue ts takes time proprtional to (length ts), which grows linearly, so the program
    is quadratic-time.
    One way to reduce this  to linear-time is to use a data structure allowing queue operations
    in amortized conatant-time.
    However, we'll use a simpler technique involving 2 lists, one reversed.
    see Burton, "An efficient functional implementation of FIFO queues", 1982.
        Gibbons, "Deriving tidy drawings of trees", 1996.
        Hood & Melville, "Real-time queue operations in pure Lisp", 1981.

    We introduce a function bftf such that..
      bftf' (ts,vs) = bftf (ts ++ reverse vs)
    Then...
-}
bftf_ql :: Forest a -> [a]
bftf_ql ts = bftf' (ts,[])
-- it's easy to calculate bftf' as...
bftf' :: (Forest a, Forest a) -> [a]
bftf' ([],[]) = []
bftf' ([],vs) = bftf' (reverse vs, [])
bftf' (Nd a us : ts, vs) = a : bftf' (ts, reverse us ++ vs)

-- in fact, bftf' is an unfold, too.
bftf'' = unfold p f g
    where p (ts,vs) = null ts && null vs
          f (t:ts,vs) = root t
          f ([],vs) = f (reverse vs, [])
          g (t:ts, vs) = (ts, reverse (kids t) ++ vs)
          g ([],vs) = g (reverse vs, [])
