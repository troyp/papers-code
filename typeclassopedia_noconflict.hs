-- *******************
-- * TYPECLASSOPEDIA *
-- *  Brent Yorgley  *
-- *******************

import Control.Functor.Pointed

-- ---------
-- Functors.
-- ---------

{-  Functors may be thought of as either..
    1. Containers which can be mapped across; or
    2. Contexts in which to perform a computation
Functor f where
  fmap :: (a -> b) -> f a -> f b                -}

--instance Functor (Either e) where
--    fmap f (Left s) = Left s
--    fmap f (Right x) = Right (f x)
type EitherStr = Either String
ex1a, ex1b :: EitherStr Int
ex1a = Left "error"
ex1b = Right 5
ex1aresult = fmap (*2) ex1a
ex1bresult = fmap (*2) ex1b

--instance Functor ((,) e) where
--    fmap f (s,x) = (s,f x)
type Annotate = ((,) String)
ex2 :: Annotate Int
ex2 = ("example Annotate Int value", 7)
ex2result = fmap (*2) ex2

--instance Functor ((->) e) where
--    fmap f mapping = f . mapping
type IntMapping = ((->) Int)
ex3 :: IntMapping Int
ex3 = (2*)
ex3result = fmap (1+) ex3 $ 1
ex3b = fmap (*2) length $ "hello"    -- apples fmap (*2) to the func length :: String->Int
ex3c = fmap (++"!") tail $ "hello"   -- applies fmap (++"!") to func tail: String->String
-- "tail" is in the same functor as "length", ie. the Functor (String->)

{-  Sensible Functors should obey the Functor Laws...
    1. fmap id = id
    2. fmap (g . h) = fmap g . fmap h                -}

{-  Intuition.
    ----------
    We can think of fmap as...
    1. taking 2 parameters, a function and a container, and applying the funciton "inside"
       the conatiner, creating a new container;
    2. applying a funciton to a value in a context, without altering the context.
    * Like all Haskell functions, fmap is actually curried. It truly takes a single parameter,
    a function :: a->b, and returns a function :: f a->f b. Written such, it is clear that fmap
    *transforms* a function: it takes a "normal" function (g :: a-> b) and returns a function
    which operates over containers/contexts (fmap g :: f a -> f b). This transformation is 
    called "lifting": fmap lifts a function from the "normal world" into the "f world".    -}



-- --------
-- Pointed.
-- --------

class Functor f => Pointed f where
    pure :: a -> f a    -- aka singleton, return, unit, point
instance Pointed ((->) e) where
    pure g = id
