{-# LANGUAGE TypeSynonymInstances, TypeOperators #-}

-- *******************
-- * TYPECLASSOPEDIA *
-- *  Brent Yorgley  *
-- *******************

import Data.Monoid
import qualified Control.Monad as M
import Data.Maybe

-- ---------
-- Functors.
-- ---------

{-  Functors may be thought of as either..
    1. Containers which can be mapped across; or
    2. Contexts in which to perform a computation
Functor f where
  fmap :: (a -> b) -> f a -> f b                -}

-- instance Functor (Either e) where
--     fmap f (Left s) = Left s
--     fmap f (Right x) = Right (f x)
type EitherStr = Either String
ex1a, ex1b :: EitherStr Int
ex1a = Left "error"
ex1b = Right 5
ex1aresult = fmap (*2) ex1a
ex1bresult = fmap (*2) ex1b

-- instance Functor ((,) e) where
--     fmap f (s,x) = (s,f x)
type Annotate = ((,) String)
ex2 :: Annotate Int
ex2 = ("example Annotate Int value", 7)
ex2result = fmap (*2) ex2
-- instance Functor ((->) e) where
    -- fmap f mapping = f . mapping
{-  Functions that take a parameter of type e. As a container, we can think of this Functor as a
    (possible infinite) set of values indexed by values of e. Alternatively, it can be thought
    of as a context in which a value of type e is available to be consulted in a read-only
    manner (hence, this is sometimes called the "reader monad".                              -}
type IntMapping = ((->) Int)
ex3 :: IntMapping Int
ex3 = (2*)
ex3result = fmap (1+) ex3 $ 1
ex3b = fmap (*2) length $ "hello"    -- apples fmap (*2) to the func length :: String->Int
ex3c = fmap (++"!") tail $ "hello"   -- applies fmap (++"!") to func tail: String->String
-- "tail" is in the same functor as "length", ie. the Functor (String->)

{-  IO is a functor: a value of type IO a represents a computation representing a value of
    type a which may have IO effects. If m computes the value x while producing some IO effects,
    fmap g m will compute the value (g x) while producing the same effects.                   -}

{-  Many types from the containers library are functors. However, note that while sets are
    mathematical functors, the Set type is not a haskell Functor since it has an Ord constraint
    on its elements; fmap must be applicable to *any* types a and b.                         -} 

{-  Sensible Functors should obey the Functor Laws...
    1. fmap id = id
    2. fmap (g . h) = fmap g . fmap h                -}
-- the following is a legal instance, but breaks both Functor laws and should *NOT* be used...
data BFList t = BFnil | BFCons t (BFList t) deriving (Show, Eq)
instance Functor BFList where
    fmap _ BFnil = BFnil
    fmap g (BFCons x xs) = BFCons (g x) (BFCons (g x) (fmap g xs))

{-  Intuition.
    ----------
    We can think of fmap as...
    1. taking 2 parameters, a function and a container, and applying the funciton "inside"
       the container, creating a new container;
    2. applying a function to a value in a context, without altering the context.
    * Like all Haskell functions, fmap is actually curried. It truly takes a single parameter,
    a function :: a->b, and returns a function :: f a->f b. Written such, it is clear that fmap
    *transforms* a function: it takes a "normal" function (g :: a-> b) and returns a function
    which operates over containers/contexts (fmap g :: f a -> f b). This transformation is 
    called "lifting": fmap lifts a function from the "normal world" into the "f world".    -}



-- --------
-- Pointed.
-- --------
-- A Pointed Functor is one with the additional ability to put a value into a "default context"

class Functor f => Pointed f where
    pure' :: a -> f a    -- aka singleton, return, unit, point
instance Pointed Maybe where
    pure' = Just
instance Pointed [] where
    pure' x = [x]
instance Pointed ((->) e) where
    pure' x = const x
{-  An example of a functor which is not pointed is ((,) e). The type e is entirely arbitrary,
    so we have no way of generating a value of type e out of thin air.
    However, if we add an additional restriction (eg. membership in monoid), we can make this
    type pointed.                                                                           -}
instance Monoid e => Pointed ((,) e) where
    pure' x = (mempty,x)

{-  Any Pointed instance must obey the following law...
    fmap g . pure = pure . g    -- ie. pure is a natural transformation from the identity
                                   functor to f
    This law is a "free theorem": it is guaranteed by parametricity, so it is not possible to
    define a Pointed instance which does not obey it (given a lawful functor and ignoring seq
    and _|_).                                       -}


-- ------------
-- Applicative.
-- ------------

{-  Applicative adds a single cabability to Pointed: the ability to apply a function which is
    itself in a context to a value in another context.
    Applicative encapsulates certain sorts of "effectful" computation in a purely functional
    way and encourages an applicative style of coding.
    It is midway between Functor and Monad.                                                 -}
class Functor f => Applicative f where
  pure :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b
-- note: fmap = (<*>) . pure
-- ie.   fmap f x = pure f  <*>  x
{-  Compare the type of (<*>) with that of ($) and fmap...
(<*>) :: f (a -> b) -> f a -> f b
fmap  ::   (a -> b) -> f a -> f b
($)   ::   (a -> b) ->   a ->   b
    (<*>) is like function application except everything is in a context. It's even closer to
    fmap: Functors allow us to apply a pure function within a computational context. Applicative
    gives us an additional tool: (<*>) lets us apply a function within a context when the
    function to be mapped is itself in a context.                                             -}
-- Laws.
-- -----
{-  There are several laws Applicative functors should obey, but the one which is important for
    developing intuition is the one relating Applicative to Functor...
fmap g x = pure g <*> x
    ie. mapping a pure function g over a context x, is the same as
        first injecting g into a context with pure, then applying it to x with (<*>)
    ie. We can decompose fmap into 2 more basic ops: 1. injection into a context
                                                     2. application within a context
    Control.Applicative also defines (<$>) as a synonym for fmap, so we can rewrite the law...
g <$> x = pure g <*> x                                                                       -}
infixl 4 <$>
(<$>) :: Functor f => (a->b) -> f a -> f b
(<$>) = fmap
-- Instances.
-- ----------
--  Most instances of Functor can be made instances of Applicative...
instance Applicative Maybe where
--  pure :: a -> Maybe a
    pure = Just
--  (<*>) :: Maybe (a->b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    Just f <*> Nothing = Nothing
    Just f <*> Just x = Just (f x)
{-  Lists: the list type constructor can be sensibly made Applicative in 2 ways: essentially,
    it depends on whether we consider lists as ordered collections of elements, or as contexts
    representing multiple results of a nondeterministic computation.                         -}
-- Lists as collections.
-- ---------------------
-- Since the default instantiation uses the context POV, we define a newtype wrapper, ZipList...
newtype ZipList a = ZipList { getZipList :: [a] } deriving (Eq, Show)
instance Functor ZipList where
    fmap g (ZipList xs) = ZipList (fmap g xs)
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)
-- Lists as contexts for nondeterministic computation.
-- ---------------------------------------------------
{-  This is the default implementation of Applicative lists.
    Instead of applying functions to inputs pairwise, we apply each function to all the 
    inputs in turn, and collect the results in a list.                                -}
instance Applicative [] where
    pure x = [x]
    gs <*> xs = [g x | g <- gs, x <- xs]
-- this lets us write nondeterministic computations in a natural style...
-- eg. add the result of a computation which may be 2,3 or 4, to 4...
ex4a = pure (+) <*> [2,3,4] <*> pure 4
-- or, more idiomatically...
ex4b = (+) <$> [2,3,4] <*> pure 4
-- Other Applicative instances.
-- ----------------------------
{-  * IO: behaves as you'd expect. When we execute...
          g <$> m1 <*> m2 <*> m3   the effects from the mi's occur in order from left to right.
    * ((,) a) is Applicative when a is a Monoid instance. The a values are computed in 
      parallel with the computation.
    * The Applicative module defines a Const type constructor. A value of type Const a b
      simply contains an a value. For any Monoid a, (Const a b) is an Applicative.
      This instance is useful in conjunction with things like Foldable (see below).
    * The WrappedMonad and WrappedArrow newtypes convert any instance of Monad or Arrow, resp,
      into an instance of Applicative. Both types are strictly more expressive than Applicative
      in the sense that the Applicative methods can be implemented in terms of their methods. -}
newtype WrappedMonad m a = WrapMonad {unwrapMonad :: m a}
instance Monad m => Functor (WrappedMonad m) where
    fmap f (WrapMonad ma) = WrapMonad (ma >>= return . f)
instance Monad m => Applicative (WrappedMonad m) where
    pure = WrapMonad . return
    (WrapMonad mf) <*> (WrapMonad ma) = WrapMonad (mf `ap` ma)
-- Intuition.
-- ----------
{-  McBride & Patterson use the "double-brackets" notation to denote function application
    in a computational context.
    If each xi has type f ti for an applicative functor f, and g has type t1->t2->..->tn->t,
    then [| g x1 x2 .. xn |] has type (f t).
    ie. This notation allows application of a function to multiple effectful args.
    It generalises fmap, which applies a functin to a single effectful arg.
    Why do we need Applicative to implement this generalisation of fmap?
    When we use fmap to apply g to the first arg x1, we get something of type
    f (t2->..->tn->t)
    But now we're stuck: This result is the partially-applied function which must be applied
    to the remaining args, but it's in a computational context, and fmap can't handle a 
    functin which is itself in a context.
    Hence, we need Applicative: <*> allows us to apply a function-in-a-context to an
    argument-in-a-context. This suggests the translation of double-brackets into Haskell...
    [| g x1 .. xn |]  translates to ...
    g <$> x1 <*> .. <*> xn    or    pure g <*> x1 <*> .. <*>.
    This is what we mean by "applicative style": effectful computations can still be
    represented in terms of function application; the only difference is that we must
    use the special operator <*> rather than normal function application (juxtaposition/$) -}

-- Further Reading.
-- ----------------
{-  There are many other useful combinators in the std libs: *> , <* , <**> , <$ , etc.
    McBride & Paterson's paper is the best resource.
    Conal Elliot's blog often talks about Applicative and his Pan and FRP libraries make key use
      of it. Also, his TypeCompose library allows in many cases, Applicative instances for
      complex types to be built out of the instances for the simpler types they're built out of.
      This is possible due to the properties of Applicative, including the fact that Applicative
      types are closed under composition.
    Although the Parsec library was designed for use as a monad, in its most common use cases,
      an Applicative instance can be used instead. See Bryan O'Sullivan's blog post.    -}



-- ------
-- Monad.
-- ------

{-  Monads are another typeclass that can represent effectful computation (as well as a
    "container" representation), similar to Applicative.
    The Monad Typeclass adds a singe capability to Applicative Functors: a "bind" (>>=)
    operation which can use the results of a computation to determine the next computation to
    be performed.
    Monad is not actually a subclass of Applicative (or even Functor). "pure" is renamed 
    "return" in Monad.
    return and bind form the minimal definition of a monad.
    fmap and (<*>) can be defined in terms of these ops and are provided in Control.Monad under
    different names. "fmap" is called "liftM"; (<*>) is called "ap".
    
    Monads are special in a couple of ways...
    1. The IO Monad is the framework for Haskell I/O.
    2. A special syntax (do-notation) is provided for working within a monadic context.    -}

class Monad1 m where
    -- required
    return' :: a -> m a
    (>>=~)  :: m a -> (a -> m b) -> m b
    -- default implementations
    (>>~) :: m a -> m b -> m b
    m >>~ n = m >>=~ \_ -> n
    fail' :: String -> m a
    fail' s = error s
{-  notes: 1. the simplest type-correct implementation of >> would be _>>n = n, but this does 
              not satisfy the intended semantics - m>>n should perform ignore the result of m,
              but not its effects.
           2. error has type :: (String -> a) - it raises an exception, so it can have any
              result type you wish.                                                      -}
-- alternative implementation as Applicative subclass...
class Applicative m => Monad' m where
    (>>>=) :: m a -> (a -> m b) -> m b

-- Instances.
-- ----------
-- the simplest monad is the Identity Monad...
newtype Identity a = Identity { runIdentity :: a }
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity x = Identity (f x)
instance Monad' Identity where
    Identity x >>>= f  =  f x
instance Monad Identity where
    return x = Identity x
    Identity x >>= f  =  f x
-- the next simplest is Maybe...
instance Monad' Maybe where
    -- return/pure = Just
    Just x  >>>= g  =  g x
    Nothing >>>= g  =  Nothing
{-  Looking at the definition of bind for Maybe, we can see what's going on when the Maybe monad
    is used as a computational context. If we string together a bunch of computations with bind,
    the overall computation will succeed only if every individual step succeeds. If any step
    yields Nothing, the Nothing will propagate across every >>=. Thus, the Maybe monad models
    computations that may fail.                                                             -}
-- the list monad is similar to its standard Applicative instance...
instance Monad' [] where
    (x:xs) >>>= g  =  g x ++ xs >>>= g
    -- or  ys >>= g  =  foldr ((++) . g) [] ys
{-  An important - and unique - monad is the IO monad.
    The IO Monad is magical and its implementation varies from compiler to compiler.
    Note that the IO monad is the *only* monad that is magical.
    The IO monad allows us to build up, in a pure way, values representing possibly effectful
      computations.
    The special value main::IO() is taken by the runtime and executed, producing actual effects.
    Every other monad is functionally pure and requies no special compiler support.
    Other monads may *model* effectful computations, but are actually pure.    -}
-- the reader monad ((->) e) describes computations where a value ::e is available as a
--   read-only environment. Note: we may write this type informally as (e->)

-- instance Monad ((->) e) where
--     return x = const x
--     fa >>= g = \e -> let a = fa e
--                      in (g a) e

{-  The Writer Monad allows information to be collected as a computational process.
    It is isomorphic to (a,w) where the annotation/log type w is a Monoid.
    The special function "tell" performs logging.                        -}
{-  Control.Monad.State provides the newtype (State s a), a wrapper around s->(a,s).
    (State s) is a monad instance. Something of type (State s a) represents a stateful
      computation that produces a val of type a, but has read/write access to a state var
      of type s along the way.                                                          -}
{-  Control.Monad.Cont provides the Cont monad, representing computations in CPS.
    This allows complex control flow constructs to be implemented in a functionallt pure way -}

-- Intuition.
-- ----------
{-  The key is the type of bind, (>>=) :: m a -> (a -> m b) -> m b
    Intuitively, bind combines two computations. Its first arg has type (m a). Now, if the
    second arg had type (m b), there'd be no way for the computations to interact. The second
    computation must have read-access to the result of the first. Thus, the second arg is of
    type (a -> m b) - it uses the result of the first computation to decide what computation
    to perform, finally producing a value of type b (still within the context m).
    Intuitively, it is this ability to use the output of previous computations to decide what
    to do next, that makes Monad strictly more powerful than Applicative.
    The structure of an Applicative.computation is fixed, whereas that of a Monadic computation
    can change based on intermediate results.                                               -}
{-  Another point-of-view on the increased power of Monad...
    Imagine we were trying to implement >>= in terms of fmap, pute and <**>...
    We're given a value   x :: m a   and a function   k :: a -> m b   . The only thing we can
    do is apply k to x. We can't apply it directly, so we need to use fmap to lift it over m.
    The type of the resulting function is   fmap k :: m a -> m (m b)   . Now we're stuck.
    We need a value of type  :: m b  but we have one of type  :: m (m b)  . We have no way to
    convert it to the right type. We can use pure to add "m"s, but we have no way to *collapse*
    multiple "m"s into one, which is what we need.
    This ability to collapse multiple m's is provided by the monadic function
      join :: m (m a) -> m a   . join can be defined with bind, or vice versa.
    In Category Theory, monads are defined in terms of return, fmap and join (or eta, T and mu).
    Haskell uses the formulation in terms of bind since it is more convenient to use.
    However, it can be easier to think about the formulation in terms of join, since it's a
    more "atomic" operation.
    An alternative Haskell definition in terms of join...                                  -}
class Applicative m => Monad'' m where
    join :: m (m a) -> m a

-- Utility Functions.
-- ------------------
-- exercise: implement the following utility functions...
-- join :: Monad m => m (m a) -> m a
join' :: Monad m => m (m a) -> m a
join' = (>>= id)
join'' :: Monad m => m (m a) -> m a
join'' mmx = do
  mx <- mmx
  mx
-- liftM :: Monad m => (a->b) -> m a -> m b    -- this is the same as fmap, but since Monad
--   doesn't require a Functor instance, we have both fmap and liftM.
liftM :: Monad m => (a->b) -> m a -> m b
liftM f x = x >>= (return . f)
-- ap :: Monad m => m (a->b) -> m a -> m b    -- this is the same as (<*>).
-- Any monad can be made into an instance of Applicative with pure=return, and (<*>)=ap
ap :: Monad m => m (a->b) -> m a -> m b
ap mf x = mf >>= \f -> x >>= \a -> return (f a)
ap' :: Monad m => m (a->b) -> m a -> m b
ap' mf x = do
  f <- mf
  a <- x
  return (f a)
-- sequence :: Monad m => [m a] -> m [a]   takes a list of computations and combines them into
--   a single computation that returns a list of their results. Note that sequence could be
--   implemented requiring only Applicative, for historical reasons it requires Monad.
-- Note: there's a generalisation of "sequence" to structures other than lists (see Traversable)
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do
  a <- x
  as <- sequence' xs
  return (a:as)
sequence'' :: Monad m => [m a] -> m [a]
sequence'' [] = return []
sequence'' (x:xs) = x >>= \a -> sequence'' xs >>= \as -> return (a:as)
-- replicateM :: Monad m => Int -> m a -> m [a]   is just a combination of replicate and sequence
replicateM :: Monad m => Int -> m a -> m [a]
replicateM 0 _ = return []
replicateM n x = do
  a <- x
  as <- replicateM (n-1) x
  return (a:as)
replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n = sequence . replicate n
-- when :: Monad m => Bool -> m () -> m ()   conditionally executes a computation
-- if the first arg is True, it executes the second arg, otherwise it just returns 'return ()"
when :: Monad m => Bool -> m () -> m()
when pred action = if pred then action else return ()
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]   maps a monad-returning func over a list and
--   sequences the result
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' k = sequence . map k
{- forM :: [a] -> (a -> m b) -> m [b]   is just mapM with its args reversed...
     mapM k xs == forM xs k   . This variant is called "forM" because it generalises a for-loop:
     the list ::[a] ptovides the loop indices and the function :: a->m b  specifies the loop
     "body" for each index.                                                                -}
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM
-- example of forM...
print1toN n = forM [1..n] $
              \i -> putStr ( show i ++

                             (if i/=n then " "  else "\n") ++
                             (if i`mod`10 == 0 then "\n" else "") )
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b   is just bind with its args reversed.
-- This order is sometimes more convenient as it corresponds to function application
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c   is sort of like function
--   application, but with an extra m on the result type of each function and the args swapped
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(f >=> g) a = f a >>= g
-- many of these functions have "underscored" variants, eg. mapM_, which discard the results
--   of the computations, using them only for side effects.

-- Laws.
-- -----
{-  1.  return a >>= k  =  k a
    2.  m >>= return    =  m
    3.  m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
    [additional law for monads that are also functors...]
    4.  fmap f xs  =  xs >>= return . f  =  liftM f xs
    The first two laws express the fact that return behaves nicely.
    The third law expresses the (sort-of) associativity of bind.
    The final law states that if m is both a Monad and a Functor instance, then fmap==liftM  -}
{-  The monad laws can be stated much more clearly using >=> rather than bind...
    1.  return >=> g  =  g
    2.  g >=> return  =  g
    3.  (g >=> h) >=> k  =  g >=> (h >=> k)
    The first two laws now state that return is the identity of >=>
    the 3rd says that >=> is associative.                                                     -}

-- Do Notation.
-- ------------
{-  Do-notation is recursively translated into monadic ops as follows...
      do e                    --->   e
      do {e;stmts}            --->   e >> do {stmts}
      do {v <- e; stmts}      --->   e >>= \v -> do {stmts}
                              --->   e >>= fail ...    (if v is a pattern and e doesn't match)
      do {let decls; stmts}   --->   let decls in do {stmts}                                -}
{-  Note: do-notation is designed around the context-POV, so x<-m suggests "extracting" a single
          x from the context m and doing something with it.
          However, m could represent a container such as a list or tree, and the meaning of x<-m
          depends on the implementation of >>=. If m is a list, for ex, x will take on each
          value from the list in turn (nondeterministic choice monad).                    -}

-- Monad Transformers.
-- -------------------
{-  Often we would like to combine two monads for the effects of both.
    Monads don't compose as nicely as Applicatives, so the MTL provides monad transformers that
      can be applied to other monads to produce a monad with the effects of both.
    Order of composition: monad transformers compose "inside out": MaybeT (State s) a  is a
      Maybe monad wrapped in a State monad, ie. it's isomorphic to  s -> Maybe (a,s)
    lambdabot has an @unmtl command to "unpack" such monad transformer stacks.
    All monad transformers implement the MonadTrans type class, allowing arbitrary computations
      in the base monad m to be lifted into computations in the transformed monad (m t).
    Note that type application left-associates, like function application, so  t m a == (t m) a
    You don't need to worry about MonadTrans when using predefined monad transformers        -}
class MonadTrans t where
    lift :: Monad m => m a -> t m a
-- see "monad trfmrs step-by-step", "grok monad trfmrs", "how to use monad trfmrs"

-- MonadFix.
-- ---------
{-  The class Monadfix contains monads supporting the special op mfix :: (a -> m a) -> m a
    allowing the output of monadic computations to be defined recursively.
    See erkok, "Value recursion in monadic computations".                                 -}


-- -------
-- Monoid.
-- -------
{- A monoid is a set which is closed under an associative binary operation and contains an
   identity element (left & right) for the operation.
   A monoid where every element has an inverse is a group.                               -}
class Monoid' a where
    mempty' :: a
    mappend' :: a -> a -> a
    mconcat' :: [a] -> a
    mconcat' = foldr mappend' mempty'
-- mempty and mappend are named after the list instance, but are misleading for many monoids

-- Laws.
-- -----
{- mempty `mappend` x == x `mappend` mempty == x                 -- identity
   (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)    -- associativity           -}

-- Instances.
-- ----------
-- lists are monoids under (++)
instance Monoid' [t] where
    mempty' = []
    mappend' = (++)
{-  We can make any numeric type a monad under either addition or multiplication.
    Since we can't have two instances for the same type, Data.Monoid provides newtype wrappers
      Sum and Product, with corresponding monoid instance defns.                             -}
ex_monoid_1 = getSum (mconcat . map Sum $ [1..5])
ex_monoid_2 = getProduct (mconcat . map Product $ [1..5])
-- these examples are silly, but the newtypes are useful in some circumstances (see foldable)
{-  Similarly, Bool is a monoid under either disjunction or conjunction.
    Any and All are newtype wrappers providing Monoid instances under (||) and (&&) resp.   -}
ex_monoid_3 = All True `mappend` All False
ex_monoid_4 = Any True `mappend` Any False
{-  There are 3 instances for Maybe...
    * a basic instance which lifts a Monoid instance for a to an instance for Maybe a
    * newtype wrappers First and Last, for which mappend selects the first (resp. last)
      non-Nothing item.                                                               -}
newtype First' a = First' { getFirst' :: Maybe a }
newtype Last' a = Last' { getLast' :: Maybe a }
instance Monoid (First' a) where
    mempty = First' Nothing
    r@(First' (Just x)) `mappend` _ = r
    First' Nothing `mappend` r = r
instance Monoid (Last' a) where
    mempty = Last' Nothing
    _ `mappend` r@(Last' (Just x)) = r
    r `mappend` Last' Nothing = r
--  Endo is a newtype wrapper for functions :: a->a, which form a Monoid under ($)
{-  There are several ways to "lift" Monoid instances to instances with additional structure.
    We saw above that a monoid a can be lifted to a monoid (Maybe a)
    There are tuple instances: if a and b are monoids, so is (a,b) with the op applied pairwise
    If a is a Monoid, so is (e -> a) for any e.
      In particular, g`mappend`h is the function which applies g and h to the arg (::a)
      individually, then combines the result using the underlying monoid instance for a  -}
instance Monoid' a => Monoid' (e -> a) where
    mempty' = const mempty'
    (f`mappend'`g) x = f x `mappend'` g x
{-  The type Ordering = LT|EQ|GT is a monoid. The operation is defined so that x`mappend`y
    returns the leftmost non-EQ arg, or EQ if both are EQ. The consequence of this is that the
    lexicographic ordering of lists xs and ys can be calculated with...                      -}
lexicographic :: [Ordering] -> [Ordering] -> Ordering
lexicographic xs ys = mconcat (zipWith compare xs ys)
-- There are also Monoid instances for several containers incl. Map, Set and Sequence.

-- Monoid is also used to enable several other type class instances...
-- We can use Monoid to make ((,) e) an instance of Applicative...
instance Monoid e => Applicative ((,) e) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u`mappend`v, f x)
{-  Monoid can similarly be used to make ((,) e) an instance of Monad.
    This is the "writer monad". Writer and WriterT are a newtype wrapper and trandformer, resp.,
      for this monad.                                                                         -}
-- Monoid plays a key role in Foldable (see below).

-- Other Monoidal Classes: Alternative, MonadPlus, ArrowPlus.
-- ----------------------------------------------------------
-- The Alternative type class is for Applicative Functors that also have monoidal structure...
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
{- Similarly, monads and arrows which have monoidal structure are represented by the MonadPlus
   and ArrowPlus classes.
   All these classes should obey the monad laws, of course.   -}
class Monad1 m => MonadPlus' m where
    mzero' :: m a
    mplus' :: m a -> m a -> m a
{- Instances of MonadPlus should also obey these additional laws...
   (*)  mzero >>= f  =  mzero
   (*)  v >> mzero   =  mzero
   MonadPlus is used to represent choice and failure.
   mzero represents failure as seen in the laws above.
   mzero is the identity element of the monad, so (m1 `mplus` m2) succeeds (ie. !=mzero) iff
     at least one of m1,m2 succeeds.
   Thus, mplus represents choice.
   examples: [] is an example of a type whose MonadPlus instance which matches its Monoid 
     instance. In general, though, a type may have a MonadPlus instance different from its
     Monoid instance: an example of this is Maybe.
     see Auclair, "MonadPlus: What a super Monad!", TMR 11.
   The function "guard" can be used with MonadPlus instances: it requires a condition to be
     satisfied and fails with mzero if it is not...    -}
guard' :: MonadPlus' m => Bool -> m()
guard' pred = if pred then return' () else mzero'

{-  There used to be a MonadZero class equivalent to the current Monad, and Monad included only
    return, >>= and >>.
    fail was added to Monad as a hack to allow pattern matching in do-blocks (pattern matching
    requires "fail" since a match may fail).    -}
class Arrow' arr => ArrowZero arr where
    zeroArrow :: b `arr` c
class ArrowZero arr => ArrowPlus arr where
    (<++>) :: (b `arr` c) -> (b `arr` c) -> (b `arr` c)


-- ---------
-- Foldable.
-- ---------

{- Foldable abstracts over containers that may be "folded" into a summary value.
   This allows folding to be generalised to a container-agnostic operation.
   Only one method is required to create a Foldable instance: either foldr or foldMap: each of
   these methods is defined in terms of the other. Other methods are defined in terms of foldr,
   so they need only be included if a more effiecient implementation is possible.    -}
class Foldable' t where
    fold'    :: Monoid m => t m -> m
    fold' = foldMap' id
    foldMap' :: Monoid m => (a -> m) -> t a -> m
    foldMap' f = foldr'' (mappend . f) mempty
    foldr''  :: (a -> b -> b) -> b -> t a -> b
    foldr'' f init x = appEndo (foldMap' (Endo . f) x) init
    foldl''  :: (a -> b -> a) -> a -> t b -> a
    foldl'' f init x = appEndo (getDual (foldMap' (Dual . Endo . flip f) x)) init
    foldr1'  :: (a -> a -> a) -> t a -> a
    foldr1' f xs = fromMaybe (error "foldr1: empty structure") (foldr'' mf Nothing xs)
        where mf x Nothing  = Just x
              mf x (Just y) = Just (f x y)
    foldl1'  :: (a -> a -> a) -> t a -> a
    foldl1'  f xs = fromMaybe (error "foldl1: empty structure") (foldl'' mf Nothing xs)
        where mf Nothing y  = Just y
              mf (Just x) y = Just (f x y)

-- Instances and Examples.
-- -----------------------
{- We can see what foldMap does by its type: given a way to convert the data in a container to
   a Monoid (ie. a function ::a->m) and a container, foldMap iterates over the contents of the
   container, converting the a's to m's and combining the m's with mappend...    -}
instance Foldable' [] where
    foldMap' g = mconcat . map g

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
instance Foldable' Tree where
    foldMap' f Empty = mempty
    foldMap' f (Leaf x) = f x
    foldMap' f (Node l x r) = foldMap' f l `mappend` f x `mappend` foldMap' f r

{- The Foldable module also provides Maybe and Array instances, and many data structures from
   the standard containers library define their own instances, eg. Map, Set, Tree, Sequence. -}

-- Derived Folds.
-- --------------
{- We can write generic, container-agnostic functions that apply to all Foldable instances.
   The Foldable module provides many predefined folds, many of which are generalized versions
   of the list-folding functions in the Prelude, eg. concat, concatMap, and, or, any, all, sum,
   product, maximum(By), minimum(By), elem, notElem, find.    -}
containerSize :: Foldable' f => f a -> Int
containerSize = getSum . foldMap' (const (Sum 1))
filterF :: Foldable' f => (a -> Bool) -> f a -> [a]
filterF p = foldMap' (\a -> if p a then [a] else [])
filterF2 :: Foldable' f => (a -> Bool) -> f a -> [a]
filterF2 p = foldr'' (\x z -> if p x then (x:z) else z) []
-- example of a specific fold: get a list of all strs in a container including the letter 'a'...
aStrings :: Foldable' f => f String -> [String]
aStrings = filterF (elem 'a')

{- There are also generic functions working with Applicative or Monad instances to generate some
   kind of computation from each element in a container, and then perform all the side-effects
   from the computations, discarding the results, eg. traverse_, sequenceA_.
   The reason the results must be discarded is because the Foldable class is not strong enough
   to be able to collect them in general: we can't turn an arbitrary Applicative or Monad 
   instance into a Monoid. If we do have a Monoid (ie. an Alternative or MonadPlus instance),
   we can use asum or msum, which collect the results as well.    -}

{- Note that the Foldable operations always forget the structure of the container they fold over.
   Thus, any function in Foldable operating on type t a for Foldable t, will never have t in the
   output type.
   Often this is what we want, but sometimes we'd like to be able to traverse a container while
   preserving its structure: this capability is provided by the Traversable type class.    -}

-- some generalisations of Prelude list functions...
concatF :: Foldable' t => t [a] -> [a]
concatF = fold'
concatMapF :: Foldable' t => (a -> [b]) -> t a -> [b]
concatMapF f = foldMap' f
andF :: Foldable' t => t Bool -> Bool
andF = getAll . foldMap' All
orF :: Foldable' t => t Bool -> Bool
orF = getAny . foldMap' Any
anyF :: Foldable' t => (a -> Bool) -> t a -> Bool
anyF p = getAny . foldMap' (Any . p)
allF :: Foldable' t => (a -> Bool) -> t a -> Bool
allF p = getAll . foldMap' (All . p)
sumF :: (Foldable' t, Num a) => t a -> a
sumF = getSum . foldMap' Sum
productF :: (Foldable' t, Num a) => t a -> a
productF = getProduct . foldMap' Product
maximumF :: (Foldable' t, Ord a) => t a -> a
maximumF = foldr1' max
minimumF :: (Foldable' t, Ord a) => t a -> a
minimumF = foldr1' min
maximumBy :: Foldable' t => (a -> a -> Ordering) -> t a -> a
maximumBy f = foldr1' (\x z -> if f x z == GT then x else z)
minimumBy :: Foldable' t => (a -> a -> Ordering) -> t a -> a
minimumBy f = foldr1' (\x z -> if f x z == LT then x else z)
{- note: version in GHC takes the leftmost of equal-minimum elements, although the maximumBy
         function takes the rightmost. I've used the rightmost in both functions.    -}
elemF :: (Foldable' t, Eq a) => a -> t a -> Bool
elemF a = getAny . foldMap' (Any . (== a))
-- or just..
elemF2 :: (Foldable' t, Eq a) => a -> t a -> Bool
elemF2 = anyF . (==)
notElemF ::  (Foldable' t, Eq a) => a -> t a -> Bool
notElemF = allF . (/=)
findF :: Foldable' t => (a -> Bool) -> t a -> Maybe a
findF p = listToMaybe . concatMapF (\x -> if p x then [x] else [])
-- testing...
exBoolTree1 = Node (Leaf True) True (Node (Node (Leaf True) True (Leaf True)) True (Leaf True))
exBoolTree2 = Node (Leaf True) True (Node (Node (Leaf True) True (Leaf False)) True (Leaf True))
exIntTree1 = Node (Leaf 3) 2 (Node (Node (Leaf 1) 0 (Leaf 3)) 4 (Leaf 4))
exIntTree2 = Node (Leaf 3) 2 (Node (Node (Leaf 1) 0 (Leaf 5)) 4 (Leaf 4))
ex5a = andF exBoolTree1
ex5b = andF exBoolTree2
ex5c = orF exBoolTree2
ex6a = allF (<5) exIntTree1
ex6b = allF (<5) exIntTree2
ex6c = anyF (<5) exIntTree2
ex7a = sumF exIntTree1
ex7b = productF exIntTree1


-- -----------
-- Traversable.
-- -----------
{- Traversable lets us traverse a container while retaining its structure.
   Every Traversable instance is also a Foldable Functor. One additional method is required:
     either traverse or sequenceA.     -}
class (Functor t, Foldable' t) => Traversable' t where
    traverse' ::  Applicative f => (a -> f b) -> t a -> f (t b)
    traverse' g = sequenceA' . fmap g
    sequenceA' :: Applicative f => t (f a) -> f (t a)
    sequenceA' = traverse' id
    mapM''     :: Monad m => (a -> m b) -> t a -> m (t b)
    mapM'' g = unwrapMonad . traverse' (WrapMonad . g)
    sequenceM  :: Monad m => t (m a) -> m (t a)
    sequenceM  = mapM'' id

-- Intuition.
-- -----------
{-  The key method of Traversable is sequenceA. Consider its type...
    sequenceA :: Applicative f => t (f a) -> f (t a)
    This answers a fundametental question: When can we compose 2 functor?
    eg. Can we turn a tree of lists into a list of trees?
        Yes, in two ways (corresponding to the 2 ways to make List an Applicative instance:
        We can make a list of all possible trees formed by choosing one element of out each
        node's list; or we can make a list of trees formed by choosing the first element of each
        node's list, then the second of each, etc.).
    The ability to compose 2 monads depends crucially on being able to commute functors.
      Intuitively, if we want to compose monads m and n to form M a = m (n a), we need to be
      able to form join :: m (n (m (n a))) -> m (n a). We can do this using the join methods of
      the input monads, but only if we can commute the n past the m to get m (m (n (n a))).
      See Jones, "FP with Overloading and HigherOrder Polymorphism".     -}

-- Instances and Examples.
-- -----------------------
instance Functor Tree where
    fmap g Empty        = Empty
    fmap g (Leaf x)     = Leaf (g x)
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)
instance Traversable' Tree where
    traverse' g Empty        = pure Empty
    traverse' g (Leaf x)     = Leaf <$> g x
    traverse' g (Node l x r) = Node <$> traverse' g l
                                   <*> g x
                                   <*> traverse' g r
{- Note that the instance definitions for Functor and Tree are almost identical.
   The only difference is that the Functor method involves normal function application, while
     applications in the Traversable instance occur in an Applicative context, using <$> and <*>.
   This will be true for any type.

   Any Traversable instance is also Foldable and a Functor.
   We can implement the methods of both classes (fmap, foldMap) using the traversable methods.

   There are a number of Traversable instances in the standard libraries, including those for
     [], Maybe, Map, Tree and Sequence.
   Note that Set is *not* Traversable, although it is Foldable.      -}
 -- see also: McBride & Paterson, "Applicative Programming with Effects",
--            Gibbons & Oliveira, "The Essence of the Iterator Pattern".


-- ---------
-- Category.
-- ---------
-- Category generalises the composition of functions to general "morphisms"
-- An instance of Category should be a type constructor, ie. something of kind *->*->* ...
class Category' arr where
    id'  :: a `arr` a
    (.~) :: (b `arr` c) -> (a `arr` b) -> (a `arr` c)
-- or with a prefix type constructor...
class Category'' cat where
    id''  :: cat a a
    (.~~) :: cat b c -> cat a b -> cat a c
{- note: if we replace the type constructor varaible (cat or arr) with the function constructor
         (->). In this case, we recover the original Prelude definitions of id and (.)        -}
-- The Data.Category module includes an instance for (->), and also a (Kleisli m) instance...
instance Category' (->) where
    id' = id
    (.~) = (.)
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
instance Monad m => Category' (Kleisli m) where
    id' = Kleisli return
    Kleisli g .~ Kleisli h = Kleisli (h >=> g)
-- The module also exports operators <<< (synonym for .) and >>> (. with args reversed)...
(<<<) :: Category' arr => (b `arr` c) -> (a `arr` b) -> (a `arr` c)
(<<<) = (.~)
(>>>) :: Category' arr =>  (a `arr` b) -> (b `arr` c) ->(a `arr` c)
(>>>) = flip (.~)
infixr 1 >>>, <<<
{- The only law for Category is that id and (.) should form a monoid.
   ie.   id . f  ==  f . id  ==  f       (identity)
         f . (g . h)  ==  (f . g) . h    (associativiy)            -}

{-  Note: Category cannot represent arbitrary mathematical categories, but only categories whose
          objects are in the category Hask of Haskell types.
          For a more general treatment, see the categories-extra package.                    -}


-- ------
-- ARROW.
-- ------
{-  Arrow, like Applicative and Monad, represents an abstraction of computation.
    However, while Applicative and Monad have types that only represent their output,
    Arrow's type reflects both its input and its output.
    Arrows generalise functions.
    If arr is an Arrow instance, b`arr`c can be thought of as a computation that takes values of
      type b as input and produces values of type c as output.
    In the case of the (->) instance, this is just a pure function, but in the general case may
      represent some kind of effectful computation.                                          -}
infixr 3 ****, &&&&
class Category' arr => Arrow' arr where
    arr'    :: (b -> c) -> (b `arr` c)
    first'  :: (b `arr` c) -> ((b,d) `arr` (c,d))
    first' f = f **** (arr' id)    -- this default is not provided in Control.Arrow
    second' :: (b `arr` c) -> ((d,b) `arr` (d,c))
    second' f = arr' swap >>> first' f >>> arr' swap
        where swap (x,y) = (y,x)
    (****)  :: (b `arr` c) -> (b' `arr` c') -> ((b,b') `arr` (c,c'))
    f****g = first' f >>> second' g
    (&&&&)  :: (b `arr` c) -> (b `arr` c') -> (b `arr` (c,c'))
    f&&&&g = arr' (\x->(x,x)) >>> f****g
{-  Note that the Category class constraint gives us arrow composition and identity for free.
    Given 2 arrows g::b`arr`c and h::c~d, we can form their composition g>>>h::b`arr`d.
    Of the other operations, only arr and first need be defined for an instance.            -}

-- Intuition.
-- ----------
{-  The "arr" function takes any function ::b->c and turns it into a generalised arrow ::b`arr`c
      The intention is that arr g is "pure" in the sense that it has no "effects".
    The "first" method turns any arrow from b to c into an arrow from (b,d) to (c,d).
      The idea is that (first g) uses g to compute the first element of a pair, and lets the
      second element pass through unchanged.
      For the function instance of arrow, first g (x, y) = (g x, y).
    The "second" method is analagous to first.
      Indeed, second f = arr swap >>> first f >>> arr swap
    The (***) operation is "parallel composition" of arrows: it takes 2 arrows and makes them
      into one arrow on pairs, which has the behaviour of the first arrow on the first element
      of the pair, and the behaviour of the second arrow on the second element.
      The default iplementation of (***) is in terms of first, second and (>>>).
      Alternatively, we could define first and second in terms of (***).
    The (&&&) is "fanout composition" of arrows: it takes 2 arrows, g and h, and makes them into
      an arrow g&&&h which supplies its input to both g and h, and returns their results in a
      pair.
      The mnemonic is that g&&&h performs both g *and* h on its input, hence "&".
      For functions, (g&&&h) x = (g x, h x)                                     -}

-- Instances.
-- ----------
--  The Arrow lib itself only provides 2 instances of Arrow: (->) and (Kleisli m).
instance Arrow' (->) where
    arr' = id
    first' f (x,y) = (f x,y)
instance Monad m =>  Arrow' (Kleisli m) where
    arr' f = Kleisli (return . f)
    first' (Kleisli f) = Kleisli (\(x,y) -> f x >>= \x' -> return (x',y))

-- Laws.
-- -----
{-  There are quite a few laws that Arrow instances should obey...
    1.  arr id  =  id
    2.  arr (h . g)  =  arr g >>> arr h
    3.  first (arr g)  =  arr (g***id)
    4.  first (g>>>h)  =  first g >>> first h
    5.  first g >>> arr (id***h)  =  arr (id***h) >>> first g
    6.  first g >>> arr fst  =  arr fst >>> g
    7.  first (first g) >>> arr assoc  =  arr assoc >>> first g
            where assoc ((x,y),z) = (x,(y,z))
    There are additional rules covering ArrowChoice, ArrowApply and ArrowLoop instances.
    For more details, see Paterson, "Programming with Arrows".                         -}





-- ***********
-- *         *
-- * SCRATCH *
-- *         *
-- ***********
ex1 = fmap (++"new-message\n") (7,"existing-message\n")
