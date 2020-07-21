{-# LANGUAGE TypeOperators #-}

-- ****************************************
-- *                                      *
-- * APPLICATIVE PROGRAMIMNG WITH EFFECTS *
-- *        McBride & Patterson           *
-- *                                      *
-- ****************************************

import Control.Monad
import Data.List

-- ================
-- 1. Introduction.
-- ================

-- We discuss a pattern that crops up repeatedly. Examples...

--  Sequencing commands.
--  --------------------
{-  One often wishes to execute a sequence of commands and collect the sequence of responses.
    The Haskell Prelude includes a function sequence for this, here specialized for IO...    -}
sequence1 :: [IO a] -> IO [a]
sequence1 [] = return []
sequence1 (c:cs) = do
  x <- c
  xs <- sequence1 cs
  return (x:xs)
{-  Note that in the c:cs case, we collect the values of some effectful computations, and then
    pass them as args to the pure function (:). It would be nice if we could pass the results
    of the computations to (:) directly, without needing to bind them to names first. For this,
    we need a kind of "effectful application". Control.Monad contains such a function: ap.    -}
ap' :: Monad m => m (a->k) -> m a -> m k
ap' mf mx = do
  f <- mf
  x <- mx
  return (f x)
-- We can rewrite sequence using ap...
sequence2 :: [IO a] -> IO [a]
sequence2 [] = return []
sequence2 (c:cs) = return (:) `ap` c `ap` sequence2 cs
{-  Here, the return operation lifts pure values into the effectful world, while ap provides
    application within it.
    Note that when we effectfully apply (:) to the command c, the result is a partially-applied
    function inside the IO monad, ie. of type IO ([a]->[a]). This is why a second use of ap is
    required.
    Apart from the returns and aps, this defn is in a fairly standard applicative style, 
    although effects are present.                                                            -}

--  Transposing 'matrices'.
--  -----------------------
-- Suppose we represent matrices (approximately) as lists of lists. We write transposition...
transpose' :: [[a]] -> [[a]]
transpose' [] = repeat []
transpose' (xs:xss) = zipWith (:) xs (transpose' xss)
{-  The binary zipWith is one of a family of operations that "vectorise" pure functions.
    Fridlender & Indrika (2000) show that the whole family can be generated from "repeat",
    which generates an infinite stream from its arg, and "zapp", a kind of "zippy application".
    zapp is equivalent to (zipWith ($))                                                      -}
-- note: this func is different from that in Data.List in its treatment of ragged lists
repeat' :: a -> [a]
repeat' x = x : repeat x
zapp :: [a->b] -> [a] -> [b]
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _ _ = []
{-  The general scheme is
zipWith_n :: (a1->...->an->b) -> [a1] -> ... -> [an] -> [b]
zipWith_n f xs1 ... xsn = repeat f`zapp`xs1`zapp`...`zapp`xsn
    In particular, transposition becomes...                                                -}
transpose2 :: [[a]] -> [[a]]
transpose2 [] = repeat []
transpose2 (xs:xss) = repeat (:) `zapp` xs `zapp` transpose2 xss
{-  Apart from the repeats and zapps, this defn is in a fairly standard applicative style,
    even though we are working with vectors.                                             -}

-- Evaluating expressions.
-- -----------------------
{-  When implementing an evaluator for a language of expressions, one generally passes around
    an evironment, giving values to free variables.                                         -}
data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)
           deriving (Eq, Show)
eval1 :: (Eq v, Show v)=> Exp v -> Env v -> Int
eval1 (Var x) g = fetch x g
eval1 (Val i) g = i
eval1 (Add p q) g = eval1 p g + eval1 q g
-- where Env is some notion of environment, and fetch x projects the value of var x, eg...
type Env v = [(v, Exp v)]
fetch x g = case lookup x g of
              Just xval -> eval1 xval g
              Nothing   -> error ("no variable " ++ show x)
-- we can remove some of the clutter of explicitly-threaded environment using K and S combinators
k :: a -> env -> a
k x g = x
s :: (env -> a -> b) -> (env -> a) -> (env -> b)
s ef es g = (ef g) (es g)
-- s can be thought of as applying a function :: a->b to a value :: a within an environment env
type Env2 v = v -> Exp v
eval2 :: (Eq v, Show v)=> Exp v -> Env2 v -> Int
eval2 (Var x) = fetch2 x
eval2 (Val i) = k i
eval2 (Add p q) = k (+) `s` eval2 p `s` eval2 q
{-  Apart from the K and S combinators, this defn of eval is in fairly standard applicative
    style, even though we're abstracting an environment                                   -}
--  note: K and S are the "return and ap of the environment monad"
fetch2 x g = eval2 (g x) g


-- fmap operator (for convenience)
infixl 4 <$>
(<$>) :: Functor f => (a->b) -> f a -> f b
(<$>) = fmap
-- =========================
-- 2. The Applicative class.
-- =========================
{-  We've seen three examples of this pattern: "pure function applied to funny args". In each
    case, there's a type constructor f that embeds the usual notion of value but supports its
    own peculiar way of giving meaning to the applicative language - its *idiom*.
    To abstract this pattern, we introduce the Applicative class.                           -}
infixl 4 <*>
class Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a->b) -> f a -> f b
{-  This class generalisees S and K from threading an environment to threading an effect
    in general.
    The following laws hold for applicative functors...
    identity:      pure id <*> u = u
    composition:   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    homomorphism:  pure f <*> pure x = pure (f x)
    interchange:   u <*> pure x = pure (\f -> f x) <*> u                                -}
{-  pure embeds pure computations into the pure fragment of an effectful world - thus, the
    computations may be freely shunted around as long as the order of the genuinely effectful
    computations are preserved.
    Applicative functors are indeed functors, with the following defn of fmap...
(<$>) :: Applicative f => (a->b) -> f a -> f b
f <$> u = pure f <*> u
    Any expression built from Applicative combinators can be transformed into a canonical form
    consisting of a pure function applied to the effectful parts in depth-first order...
pure f <*> u1 <*> .. <*> un
    This canonical form captures the essence of applicative programming: computations have a
    fixed structure, given by the pure function, and a sequence of subcomputations, given by
    the effectful args.
    We represent an applicatve computation with "double-bracket" notation...
    [| f u1 .. un |] , insicating a shift into the idiom of an Applicatve functor, where a
    pure function is applied to a sequence of effectful args using the appropriate <*>.
    To make any monad (eg. IO) an instance of Applicative, we take pure=return and <*>=ap.
    Alternatively, of course, we could use the variant of ap that performs the computations
    in the opposite order, but we'll keep left-to-right order here.                       -}
instance Applicative IO where
    pure = return
    (<*>) = ap
-- sometimes, we can implement Applicative more directly...
instance Applicative ((->) env) where
    pure x = \g -> x                   -- K
    ef <*> ex = \g -> (ef g) (ex g)    -- S
-- we can now rewrite sequence and eval...
sequence' :: [IO a] -> IO [a]
sequence' [] = pure []
sequence' (c:cs) = (:) <$> c <*> sequence' cs
eval :: (Eq v, Show v)=> Exp v -> Env2 v -> Int
eval (Var x) = fetch2 x
eval (Val i) = pure i
eval (Add p q) = pure (+) <*> eval p <*> eval q
{-  if we wish to write transpose in applicative form, we must avoid the library's "list of
    successes" monad, and instead define an Applicative instance for lists that supports
    "vectorisation", where pure=repeat and (<*>)=zapp. (Note that repeat and zapp are not the
    return and ap for any monad.)                                                           -}
infixl 4 <**>
class Applicative2 f where
    pure2  :: a -> f a
    (<**>) :: f (a->b) -> f a -> f b
instance Applicative2 [] where
    pure2 = repeat
    (<**>) = zapp
transpose'' :: [[a]] -> [[a]]
transpose'' [] = pure2 []
transpose'' (xs:xss) = pure2 (:) <**> xs <**> xss
-- regular Applicative instance for lists...
instance Applicative [] where
    pure x = [x]
    gs <*> xs = [ g x | g<-gs, x<-xs ]


-- ==============================
-- 3. Traversing data structures.
-- ==============================
{-  Sequence and transpose are both instances of what we call the "applicative distributor"
    for lists...                                                                          -}
dist' :: Applicative f => [f a] -> f [a]
dist' [] = pure []
dist' (v:vs) = pure (:) <*> v <*> dist' vs
{-  Distribution is often used with map, eg. given the monadic "failure-propagation" applicative
    functor for Maybe, we can map some failure-prone function (f :: a -> Maybe b) across a list
    of inputs such that any individual failure causes overall failure...                      -}
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    (Just f) <*> (Just x) = Just (f x)
flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
flakyMap f ss = dist' (fmap f ss)
-- example...
reciprocal :: Int -> Maybe Rational
reciprocal 0 = Nothing
reciprocal n = Just (1/(fromIntegral n))
exRecip1 = flakyMap reciprocal [1..4]    -- Just [1 % 1,1 % 2,1 % 3,1 % 4]
exRecip2 = flakyMap reciprocal [0..3]    -- Nothing
{-  flakyMap traverse's ss twice: once when mapping the function over the list, and again when
    using dist to collect the results. More generally, it's preferable to define this 
    applicative mapping operation directly, with a single traversal...                      -}
traverse' :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse' f [] = pure []
traverse' f (x:xs) = pure (:) <*> (f x) <*> traverse' f xs
{-  This is just the way you'd implement the ordinary fmap for lists, but with the RHS's
    wrapped in double-brackets, shifting them into the idiom.
    Like fmap, "traverse'" is a useful tool to have for many data strutures. Hence, we define
    a type class Traversable, capturing functorial data structures through which we can thread
    an applicative computation...                                                            -}
class Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a  -> f (t b)
    dist     :: Applicative f =>            t (f a) -> f (t a)
    dist = traverse id
instance Traversable [] where
    traverse = traverse'
{-  We can recover an ordinary "map" operator by taking f to be the identity - the simple
    applicative functor in which all computations are pure...                           -}
newtype Id a = An { an::a }
{-  Haskell's newtype declarations allow us to shunt the syntax of types around without
    changing the runtime notion of value or incurring any runtime cost.
    The 'labelled field' notation defines the projection (an :: Id a -> a) at the same time as
    the constructor (An :: a -> Id a).
    The identity applicative functor has the usual application...                            -}
instance Applicative Id where
    pure = An
    An f <*> An x = An (f x)
-- so, with the newtype signalling which applicative functor to thread, we have...
fmap' f = an . traverse (An . f)
{-  All regular type constructors can be made instances of Traversable. The rule-of-thumb for
    traverse is 'like fmap but with double-brackets on the RHS'. eg...                      -}
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)
instance Traversable Tree where
    traverse f Leaf = pure Leaf
    traverse f (Node l x r) = pure Node <*> traverse f l <*> f x <*> traverse f r
{-  This construction works even for non-regular types. However, not every functor is
    traversable. For instance, the functor ((->) env) cannot in general be traversable.  -}
{-  Meertens's main goal was to generalise reduction, or "crush", operators such as
    flatterning trees and summing lists. We look at this next...                         -}


-- ============================================
-- 4. Monoids are phantom Applicative functors.
-- ============================================
-- Data that may be sensibly accumulted has a monoid structure...
class Monoid o where
    mId :: o
    (<+>) :: o -> o -> o
{-  Here, <+> is an associative operation with identity mId. There are many examples of
    monoids: numeric types (wrt 0 and +, or 1 and *), lists wrt [] and ++, etc. Hence, it is
    useful to have generic technology for working with them.                               -}
-- Every monoid induces an applicative functor, albeit in a slightly peculiar way...
newtype Accy o a = Acc { acc::o } deriving (Show, Eq)
{-  (Accy o a) is a "phantom type" (Leijen & Meijer: "Domain Specific Embedded Compilers", 1999)
    Its values are independent of a, but it yields the applicative functor of accumulating
    computations...                                                                      -}
instance Monoid o => Applicative (Accy o) where
    pure _ = Acc mId
    Acc o1 <*> Acc o2 = Acc (o1 <+> o2)
instance Monoid [a] where
    mId = []
    (<+>) = (++)
{-  Now reduction ("crushing") is just a special kind of traversal, as with other applicative
    functors, just as Meertens suggested...                                                 -}
accumulate :: (Traversable t, Monoid o)=> (a->o) -> t a -> o
accumulate f = acc . traverse (Acc . f)
reduce :: (Traversable t, Monoid o)=> t o -> o
reduce = accumulate id
-- Operations like flattening and concatenation become straightforward...
flatten :: Tree a -> [a]
flatten = accumulate (:[])
concat' :: [[a]] -> [a]
concat' = reduce
{-  We can get even more out of instance inference if we use the type system to distinguish
    different monoids available for a given datatype.
    eg. We here use the disjunctive structure of Bool to test for an element satisfying a
        given predicate...                                                                -}
newtype Mighty = Might { might::Bool }
instance Monoid Mighty where
    mId = Might False
    Might x <+> Might y = Might (x || y)
any' :: Traversable t=> (a->Bool) -> t a -> Bool
any' pred = might . accumulate (Might . pred)
{-  Now, (any . (==)) behaves just like the elem function for lists, but it can also tell
    whether a variable from v occurs free in an (Exp v).
    Of course, Bool also has a conjunctive structure, which we can exploit just as easily.. -}
newtype Musty = Must { must::Bool }
instance Monoid Musty where
    mId = Must True
    Must x <+> Must y = Must (x && y)


-- ========================
-- 5. Applicative vs Monad?
-- ========================
{-  Every monad can be made Applicative via return and ap. Two of our three introductory
    examples used the IO monad and the environment monad ((->) env).
    However, the Applicative structure we defined on lists is not monadic. Neither is (Accy o)
    for nontrivial o. return could deliver mId, but if we try to define
(>>=) :: Accy o a -> (a -> Accy o b) -> Accy o b
    we find it's difficult to extract a value ::a from the first arg to supply to the second -
    all we get is an ::o. The <*> for (Accy o) is not the ap of a monad.
    So, we see that there are strictly more Applicative functors than Monads. Should we just
    use Applicative instead of Monad? No. There are fewer monads because Monad is more
    powerful than Applicative - sometimes we need that extra power. Intuitively,...
(>>=) :: m a -> (a -> m b) -> m b
    allows the result of one computation to influence the choice of another, whereas <*> keeps
    the structure of a computation fixed, just sequencing the effects. For example...        -}
miffy :: Monad m => m Bool -> m a -> m a -> m a
miffy mb mt me = do
  b <- mb
  if b then mt else me
-- the value of mb will choose between the *computations* mt and me, performing only one.
iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy fb ft fe = pure cond <*> fb <*> ft <*> fe    --   [| cond fb ft fe |]
    where cond b t e = if b then t else e
-- performs all three computations - fb chooses only between the *value* of ft an fe
{-  This means, firstly, that iffy fails if either ft or fe fails, even though only the value
    of one is needed; and secondly, that if the computations have side effects, iffy causes
    the side effects of both ft an fe to be performed.                                    -}

-- the moral: if you've got an Applicative functor, good! If it's also a Monad, even better!
-- (dual)   : if want a Monad, good. If you only want an Applicative functor, even better!

{-  One situation where the full power of monads is often not required is parsing. Rojemo
    proposed, as an alternative to monadic parsers, an interface that included the equivalent
    of pure and <*>. Several ingenious non-monadic implementations have been developed by
    S D Swierstra et al. Because these parsers have a computation structure independent of the
    results of parsing, they are able to analyze the grammar lazily and genereate very 
    efficient parsers.                                                                -}

-- Composing Applicative Functors.
{-  The weakness of applicative functors makes them easier to construct from components.
    Only some monads can be composed, but Applicative is closed under composition...    -}
newtype (f:.:g) a = Comp { comp::f (g a) } deriving (Show, Eq)
-- we compose two functors by just lifting the inner Applicative operations to the outer layer..
instance (Applicative f, Applicative g)=> Applicative (f:.:g) where
    pure x = Comp (pure (pure x))                            -- Comp [| pure x |]
    Comp fs <*> Comp xs = Comp (pure (<*>) <*> fs <*> xs)    -- Comp [| (<*>) fa xs |]
exComp1 :: (Maybe:.:[]) Integer
exComp1 = pure 4
exComp2= pure (\x->2*x+1) <*> exComp1
{-  Hence, while two Monads may not be able to be composed as Monads, they can always be
    composed as Applicative functors.For example, both Maybe:.:IO and IO:.:Maybe are
    Applicative. IO:.:Maybe is an applicative functor in which computations have a notion
    of failure and prioritized choice, even if their real-world side-effects cannot be undone.
    Note that IO and Maybe can also be composed as Monads (though not in reverse order),
    but the applicative functor determined by the composed monads is different from the
    composed Applicative functor: the binding power of the monad allows the second IO action
    to be aborted if the first returns a failure.                                          -}

-- Accumulating Exceptions.
-- we can model exceptions with...
data Except err a = OK a | Failed err deriving (Show, Eq)
{-  A Monad instance of this type must abort the computation at the first error, as there is
    then no value to pass on to the second arg of (>==). However, with the Applicative
    interface, we can continue in the face of errors...                                   -}
instance Monoid err => Applicative (Except err) where
    pure = OK
    OK f <*> OK x = OK (f x)
    OK f <*> Failed errx = Failed errx
    Failed errf <*> OK x = Failed errf
    Failed errf <*> Failed errx = Failed (errf <+> errx)
-- we can use this to collect errors (with the list monoid), or to summarise them in some way



-- ================================
-- Applicative Functors and Arrows.
-- ================================
{-  To handle situations where monads were inappropriate, Hughes defined an interface called
    Arrows (Hughes, "Generalising Monads to Arrows", 2000).                                -}
class Arrow (~->) where
    arr   :: (a -> b) -> (a ~-> b)
    (>>>) :: (a ~-> b) -> (b ~-> c) -> (a ~-> c)
    first :: (a ~-> b) -> ((a,c) ~-> (b,c))
-- the simplest example of an arrow is ordinary ->...
instance Arrow (->) where
    arr = id
    (>>>) = flip (.)
    first f (x, y) = (f x, y)
{-  Other examples are Kleisli arrows of monads and comonads, and stream processors.
    Equivalent structures called "Freyd Categories" had been independently developed to
    structure denotational semantics - 
    Power & Robinson, "Premonoidal Categories and Notions of Computation". 1997.        -}
{-  There are similarities to the Applicative interface, with arr generalising pure.
    >>>, like <*>, doesn't allow the result of the first computation to affect the second,
    but it does feed the first result into the second computation.
    If we fix the first arg of an arrow type, we obtain an Applicative functor which
    generalises the environment functor we saw above...                                -}
newtype EnvArrow (~->) env a = Envr (env~->a) deriving (Show, Eq)
instance Arrow (~->) => Applicative (EnvArrow (~->) env) where
    pure x = Envr (arr (const x))
    Envr u <*> Envr v = Envr (u/\v >>> arr (\(f,x)->f x))
        where u/\v = arr dup >>> first u >>> arr swap >>> first v >>> arr swap
dup a = (a,a)
swap (a,b) = (b,a)
