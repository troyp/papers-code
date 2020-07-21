{-# LANGUAGE TypeSynonymInstances #-}

-- *******************************************
-- *                                         *
-- *  THE ESSENCE OF FUNCTIONAL PROGRAMMING  *
-- *              Philip Wadler              *
-- *                                         *
-- *******************************************

import Control.Monad
import Control.Monad.Identity


{-  Pure FLs offer lazy eval and equational reasoning.
    Impure FLs offer features like state, exceptions and continuations.
    Which is preferred?

    An important consideration is ease of modification.
    Pure langs make generally change easy, but sometimes a small change may require radical
    modification of a prog's structure, when an impure feature may accomplish the change easily.

    Consider an interpreter. If I want to add exception handling, I must modify the result type
    to include exception values and alter every recursive call to check for exceptions and 
    handle them appropriately. An imperative implementation would not need such restructuring.
    A similar problem occurs when adding an execution count, output instruction, etc.

    The alternative is to use a monad. If we structure the interpreter using monads, each change
    requires only cahnging the monad and making a few local changes.
    We can also use monads in cases where there is no corresponding impure feature.

    Programming with monads is reminiscent of CPS. We will explore the relationship between the
    two. In a sense, they're equivalent: CPS arises as a special case of a monad, and any monad
    may be embedded in CPS by changing the answer type.
    However, the monadic approach offers additional insight and allows finer control.        -}

-- =======================
-- 2. Interpreting Monads.
-- =======================

-- We show that monads enhance modularity, using several variations on a LC interpreter.

{-  The interpreter deals with values and terms.
    A value is either Wrong, a number or a function.
    Wrong indicates an error (unbound var, adding non-numbers, applying a non-function).
    A term is a var, a constant, a sum, a lambda expr, or an application.              -}

-- test data...
term0 = (App (Lam "x" (Add (Var "x") (Var "x")))     -- ie. ((\x. x+x) (10 + 11))
             (Add (Con 10) (Con 11)))                -- test term0 ==> "42"

{-  The interpreter is kept small, but can easily be extended with additional values, like
    bools, pairs and lists, and additional term forms, like conditional and fixpoint     -}

-- ----------------------------------------------------------------------------------------

-- CALL-BY-VALUE INTERPRETER.
-- --------------------------
type Name          = String
data Term          = Var Name
                   | Con Int
                   | Add Term Term
                   | Lam Name Term
                   | App Term Term
                   | At Position Term    -- added for variation 2
data Value         = Wrong
                   | Num Int
                   | Fun (Value -> M Value)
type Environment   = [(Name, Value)]

showval :: Value -> String
showval Wrong   = "<wrong?"
showval (Num i) = showint i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e = lookupM x e
interp (Con i) e = return (Num i)
interp (Add u v) e = interp u e >>= \a ->
                     interp v e >>= \b ->
                     add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a) : e)))
interp (App t u) e = interp t e >>= \f ->
                     interp u e >>= \a ->
                     apply f a
-- note that both the function and its arg are evaluated - call-by-value

add :: Value -> Value -> M Value
add1 (Num i) (Num j) = return (Num (i+j))
add1 _ _ = return Wrong
lookupM :: Name -> Environment -> M Value
lookupM1 x [] = return Wrong
lookupM1 x ((y,b):e) = if x==y then return b else lookupM x e
apply :: Value -> Value -> M Value
apply1 (Fun k) a = k a
apply1 _ _ = return Wrong

test :: Term -> String
test t = showM (interp t [])

--add = add1
--lookupM = lookupM1
--apply = apply1

showint = show
-- ---------------------------------------------------------------------------------------

-- What is a Monad?
-- ----------------
{-  A triple (M, unitM, bindM) of a type constructor m and 2 polymorphic functions
unitM :: a -> M a                   -- ie. return in Haskell
bindM :: M a -> (a -> M b) -> M b   -- ie. (>>=) in Haskell

    The basic idea in converting a prog into monadic form is to convert a function ::a -> b
    into a function ::a -> M b.  We call this "lifting" the function into the monad.      -}

-- two functions  k :: a -> M b  and  h :: b -> M c  can be composed thusly...
composeM k h = \a -> k a >>= (\b -> h b)
-- we can compare this to composing regular functions k::a->b and h::b->c like this...
compose k h = \a -> let b = k a in h b
{-  Thus, "bind" serves a role similar to that of a "let" expression.
    The monad laws ensure this form of composition is associative with return as left & right
    identity.                                                                               -}

{-  While type Value represents a value, type (M Value) represents a *computation*.
    The purpose of "return" is to coerce a value into a computation.
    The purpose of "bind" is to evaluate a computation, yielding a value.
    We could say that return gets us into a monad and bind get us around the monad.
    How do we get out of a monad? Generally, such an op is more ad hoc.
    For our purposes, it will suffice to provide showM :: M Value -> String                 -}

{-  By changing the defns of (M, return, >>=) and showM, and making other small changes,
    the interpreter can be made to exhibit a wide variety of behaviours, as we will now
    demonstrate...                                                                    -}


-- Variation Zero: Standard Interpreter.
-- -------------------------------------
--type M = Identity
--showM = showval . runIdentity

{-  This uses the Identity Monad from Control.Monad.Identity. Identity t is isomorphic to t.
    Wadler's orignal implementation of variation 0 used M t = t for the Identity Monad, allowing
    the interpreter to be significantly simplified. The reason is that the paper does not use
    typeclasses. Using a Monad class, we cannot in general do this.                         -}

-- Variation One: Error Messages.
-- ------------------------------
data E a = Success a | Error String
instance Monad E where
    return a = Success a
    (Success a) >>= f  =  f a
    (Error s)   >>= f  =  Error s
errorE s = Error s
showE (Success a) = "Success: " ++ showval a
showE (Error s) = "Error: " ++ s
{-  Each function in the interpreter either returns normally or yields an Error.
    If m :: E a  and  k :: a -> E b  then m>>=k acts as strict postfix application:
    If m succeeds, then k is applied to the result; if m fails, then so does the application. -}

-- To convert the interpreter..
type M = E
showM = showE
{-  We also need to replace the value Wrong with (Error ...). We can do this by replacing the
    helper procs lookupM, add and apply...                                                  -}
lookupE x [] = errorE ("unbound variable: " ++ x)
lookupE x ((y,b):e) = if x==y then return b else lookupM x e
addE (Num i) (Num j) = return (Num (i+j))
addE a b = errorE (showval a ++ ", " ++ showval b ++ " should be numbers")
applyE (Fun k) a = k a
applyE f a = errorE (showval f ++ " should be a function")

lookupM = lookupE
add = addE
apply = applyE

testErr = App (Con 1) (Con 2)    -- ==> "Error: 1 should be a number"


-- Variation Two: Error Messages with Positions.
-- ---------------------------------------------
-- Let Position be a type that indicates a place in the source text (eg. a line number)
type Position = Int
{-  We extend the Term datatype with a constructor indicating a location...
    eg. (At p (App f (At q a))) indicates that p is the position of the term (App f a),
        and that q is the position of the subterm a.                                  -}
type P a = Position -> E a
instance Monad (P a) where
    return x = \p -> (return x)
    m >>= k  = \p -> (m p)  >>= (\x -> k x p)



{-
newtype P m a = P { runP :: Position -> m a }
-- Based on E, we define a monad P that accepts a position to use in reporting errors...
instance Monad m => Monad (P m) where
    return x = P $ \p -> return x
    m >>= k  = P $ \p -> (return m >>= k)
-}
{-
type P m a = Position -> m a
instance Monad m => Monad (P m) where
    return x = \p -> return x
    m >>= k  = \p -> (return m >>= k)
-}
{-
type P a = Position -> E a

instance Monad P where
    return x = \p -> (return x)
    m >>= k  = \p -> (return m >>= k)
-}
{-
data MyList = C Int MyList | Null
instance Monad MyList where
    return x = C x Null
    (C x l) >>= k  = C (k x) (l >>= k)
-}