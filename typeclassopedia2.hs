{-# LANGUAGE TypeSynonymInstances #-}

-- *******************
-- * TYPECLASSOPEDIA *
-- *  Brent Yorgley  *
-- *******************

import Data.Monoid
import qualified Control.Monad as M

-- ---------
-- Functors.
-- ---------

{-  Functors may be thought of as either..
    1. Containers which can be mapped across; or
    2. Contexts in which to perform a computation
Functor f where
  fmap :: (a -> b) -> f a -> f b                -}

instance Functor (Either e) where
    fmap f (Left s) = Left s
    fmap f (Right x) = Right (f x)
type EitherStr = Either String
ex1a, ex1b :: EitherStr Int
ex1a = Left "error"
ex1b = Right 5
ex1aresult = fmap (*2) ex1a
ex1bresult = fmap (*2) ex1b

instance Functor ((,) e) where
    fmap f (s,x) = (s,f x)
type Annotate = ((,) String)
ex2 :: Annotate Int
ex2 = ("example Annotate Int value", 7)
ex2result = fmap (*2) ex2
instance Functor ((->) e) where
    fmap f mapping = f . mapping
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
      parallel with the computstion.
    * The Applicative module defines a Const type constructor. A value of type Const a b
      simply contains an a value. For any Monoid a, (Const a b) is an Applicative.
      This instance is useful in conjunction with things like Foldable (see below).
    * The WrappedMonad and WrappedArrow newtypes convert any instance of Monad or Arrow, resp,
      into an instance of Applicative. Both types are strictly more expressive than Applicative
      in the sense that the Applicative methods can be implemented in terms of their methods. -}

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
instance Monad ((->) e) where
    return x = const x
    fa >>= g = \e -> let a = fa e
                     in (g a) e
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
