{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, IncoherentInstances,
             FlexibleContexts, ScopedTypeVariables, EmptyDataDecls #-}

--  ****************************
--  *  DATA TYPES A LA CARTE.  *
--  *    Wouter Swierstra      *
--  ****************************

{-  We describe a technique for assembling both functions and datatypes from components, and explore
    how the same technology can be used to combine free monads (and hence to structure Haskell's
    monolithic IO monad.                                                                       -}


-- ================
-- 1. Introduction.
-- ================

-- Consider a simple evaluator for arithmetic expressions...
data Expr1 = Val1 Int | Add1 Expr1 Expr1 deriving (Eq, Show)
eval1 :: Expr1 -> Int
eval1 (Val1 n) = n
eval1 (Add1 x y) = eval1 x + eval1 y
-- once the datatype is chosen, we can define new functions over expressions...
-- eg. to render expressions as strings...
render1 :: Expr1 -> String
render1 (Val1 n) = show n
render1 (Add1 x y) = "(" ++ render1 x ++ " + " ++ render1 y ++ ")"
{-  However, what if we wish to add an operation to our expression language, say multiplication?
    Although we could extend our datatype for expressions, this would require new cases for all
    the functions we have defined to use the type.
    Phil Wadler has called this issue "the Expression Problem".
    "The goal is to define a data type by cases, where one can add new cases to the data type
     and new functions over the data type, without recompiling existing code, and while 
     retaining static type safety."
    We will examine one way to address the Expression Problem in Haskell: using the techniques
    presented, one can define datatypes, functions even certain monads in a modular fashion -}


-- =================================
-- 2. Fixing the Expression Problem.
-- =================================

{-  What should the datatype for expressions be? If the constructors are fixed in advance, we
    will run into the same problems as before. Rather than choose any particular constructors,
    we parametrize the expression datatype as follows...                                    -}
data Expr f = In (f (Expr f))
{-  We can think of type parameter f as the "signature" of the constructors.
    Intuitively, the type constructor f takes a type parameter corresponding to the expressions
    occurring as subtrees of construcors. The Expr datatype then ties the recursive knot,
    replacing the argument of f with (Expr f).                                                -}
{-  margin: 1. use of "f" for a type variable is a bit confusing since it looks like a function.
               Remember it's a type constructor.
            2. Expr f creates an infinitely nested datatype, but *only* if f always uses its type
               argument. The recursion occurs because (Expr f) is recursively passed to f as its
               type argument. However, f may *ignore* its type argument, in which case the
               recursion ends. eg. If f were a binary tree, recursion would occur with the
               branch constructor, but not the leaf constructor.                             -}

-- Examples.

-- if we wanted expressions consisting of integers only...
data Val e = Val Int  deriving (Eq, Show)
type IntExpr = Expr Val
{-  The only valid expressions would now be of the form In(Val n) for n::Int.
    The Val datatype doesn't use its type parameter e, as the constructor doesn't have any
    expressions as subtrees.                                                                -}

-- similarly, if we wanted expressions consisting only of addition...
data Add e = Add e e  deriving (Eq, Show)
type AddExpr = Expr Add
{-  Unlike the Val constructor, the Add constructor does use its type parameter e: Add takes
    2 arguments of type e.                                                                    -}

{-  Of course, neither values nor addition is very interesting on its own. The big challenge is,
    of course, to combine the ValExpr and AddExpr types somehow.

    The key idea is to **combine expressions be taking the coproduct of their signatures.**   -}

{-  The coproduct of two signatures is similar to the Either datatype, but instead of combining
    two base types, it combines two *type constructors*.                                     -}
data (f :+: g) e = Inl (f e)
                 | Inr (g e)
                 deriving (Eq, Show)
{-  An expression of type Expr (Val :+: Add) is either a value or a sum of two such expressions;
    it is isomorphic to our original Expr1 datatype.                                         -}
-- Later, we will specify :+: to be right-associative - see section 4.

{-  Combining datatypes using the coproduct of their signatures carries a price: it becomes very
    cumbersome to write expressions. Even a simple addition of two numbers becomes a monstrous
    jumble of constructors...                                                                -}
addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))
{-  Writing such expressions by hand is not an option: apart from the unacceptable complexity,
    such expressions may need to be updated if we further extend our datatype by constructing
    larger coproducts: the injections Inl and Inr may no longer be the correct injections into
    the coproduct.
    We will deal with this problem later; first we must determine how to evaluate such 
    expressioons.                                                                        -}


-- ==============
-- 3. Evaluation.
-- ==============

-- note: the types we defined to form the signatures of an Expr are both functors...
instance Functor Val where
    fmap f (Val x) = Val x   -- since Val ignores its type parameter, f can be any function
instance Functor Add where
    fmap f (Add e1 e2) = Add (f e1) (f e2)
-- The coproduct of two functors is also a functor...
instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)
-- These are crucial observations...
-- If f is a functor, we can fold over any value of type Expr f...
foldExpr :: Functor f => (f a -> a ) -> Expr f -> a
foldExpr fn (In t) = fn (fmap (foldExpr fn) t)
{-  This is a generalization of the fold for lists.
    The first argument of the fold, of type (f a -> a), is called an "algebra"
    An algebra determines how the various constructors of a datatype affect the final outcome.
    It specifies a single recursive step: turning a value of type f a into the desired result
      of type a.
    The fold uniformly applies these operations to the entire expression, accumulating the final
      result.                                                                                 -}

-- We can define and assemble algebras in a uniform way using Haskell's typeclass system.
-- We first introduce a class corresponding to the algebra we wish to define...
class Functor f => Eval f where
    evalAlgebra :: f Int -> Int
-- since our expressions consist of values and addition, we must define two instances...
instance Eval Val where
    evalAlgebra (Val n) = n
instance Eval Add where
    evalAlgebra (Add x y) = x + y
{-  In Add, the variables x and y are not expressions, but the Int results of recursive calls.
     which is why we do not apply evalAlgebra to them before adding.                        -}
-- Finally, we need to be able to evaluate composite functors built from coproducts...
instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x
    evalAlgebra (Inr x) = evalAlgebra x
-- evalAlgebra is defined for any Eval instance, so it's defined for both f and g
-- Now we can define evaluation by folding over an expression with the algebra we've defined...
eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr
{-  We can now define functions over expressions over folds.
    Furthermore, we can add new cases to our expression type (the typeclass Eval) in a modular
      way. To add a new case Mul, we simply define the datatype Mul and instantiate it as an
      instance of the classes Functor and Eval. We can then automatically use it in combination
      with the other cases in expressions we write.                                           -}

{-  One remaining issue: Writing expressions, even very simple ones, is still extremely 
    impractical.
    Fortunately, we can automate most of the overhead created by the use of coproducts...   -}


-- =========================
-- 4. Automating injections.
-- =========================

{-  The definition of addExample illustrates how messy expression can become.
    We will remedy this problem by defining "smart constructors" for addition and values.  -}

-- A first attempt...
val1 :: Int -> Expr Val
val1 n = In (Val n)
infixl 6 ?+?
(?+?) :: Expr Add -> Expr Add -> Expr Add
x ?+? y = In (Add x y)
{-  This is a step in the right direction, but (1 ?+? val1 3) will result in a type error.
    The addition smart constructor expects two expressions which must themselves consist solely
      of additions.
    We need our smart constructors to take more general expressions as arguments.             -}

{- We will define our smart constructors with the following types...
    (<+>) :: (Add :<: f) => Expr f -> Expr f -> Expr f
    val :: (Val :<: f) => Int -> Expr f                                                       -}
{-  Intuitively, we may consider the type constraint (T :<: f) to mean "any signature f that
      supports the type T". So (Add :<: f) means the signature f supports addition.
    More formally, the constraint (sub :<: sup) should only be satisfied if there exists an
      injection from (sub a) to (sup a).
    Rather than writing the injections by hand using Inl and Inr, the injections will be
      inferred using the following typeclass...                                            -}
class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a
    prj :: sup a -> Maybe (sub a)  -- partial inverse of inj: see end of section 5
-- We define only 3 instances for (:<:)...    (note: requires IncoherentInstances)
instance (Functor f) => (:<:) f f where
    inj = id
    prj = Just
-- states that :<: is reflexive.
instance (Functor f, Functor g) => (:<:) f (f :+: g) where
    inj = Inl
    prj (Inl f) = Just f
    prj _ = Nothing
-- explains how to inject any value of type (f a) into one of type (g a), regardless of g.
instance (Functor f, Functor g, Functor h, f:<:g) => (:<:) f (h :+: g) where
    inj = Inr . inj
    prj (Inr a) = prj a
    prj _ = Nothing
{-  states that if we can inject a value of type (f a) into one of type (g a), then we can
    also inject (f a) into a larger type ((h :+: g) a) by composing the first injection with
    an additional Inr (the second part of the process is like a reversed version of the 
    injection specified by the 2nd instance above).                                        -}
{-  We use coproducts in a list-like faxhion: the 3rd instance searches only the RHS of a
    coproduct. This simplifies the search, since no backtracking is required.
    However, as a result, these instances may fail to find an injection, even if one exists.
    eg. the following constraint would *not* be satisfied...
    f :<: ((f :+: g) :+: h)
    despite the fact that (Inl . Inl) would be a suitable injection.
    These limitations should not be encountered by users as long as their coproducts are not
    explicitly nested.
    To ensure suitable parsing in the absense of parentheses, we make :+: right-associative -}
infixr 6 :+:

{-  We can now define our smart constructors...
    (requires FlexibleContexts to allow contexts to contain constant constructors as well as 
     type variables).                                                                      -}
inject :: (g:<:f) => g (Expr f) -> Expr f
inject = In . inj
val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)
infixl 6 <+>
(<+>) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x <+> y = inject (Add x y)

-- Now we can easily construct and evaluate expressions
exExpr :: Expr (Add :+: Val) = val 30000 <+> val 1330 <+> val 7
exExprVal :: Int = eval exExpr
{-  Note that in the expression above, the type signature is *essential*. We exploit the type
    signature to determine the injection into a coproduct.
    Without one, the compiler cannot find an injection and gives an error.                  -}

{-  There is some overlap between the instances of the (:<:) typeclass, which is why we need to
    use an extension (IncoherentInstances). Consider the following example...                -}
inVal :: Int -> Expr (Val :+: Val)
inVal i = inject (Val i)
{-  The instance definitions justify both Inl and Inr as injections. Which should be preferred?
    It doesn't matter: the functions we define here do not inspect *where* something occurs in
    a coproduct.
    We can verify that eval (In (Inl (Val x))) and eval(In (Inr (Val x))) are equal for all x,
    as the instance of the Eval class for coproducts does not distinguish between Inl and Inr.
    ie. the result of Eval will never depend on the choice of injection.
    So although we need to enable to IncoherentInstances to compile this class, it should only
    result in unpredictable behaviour if we abuse the information we have about the order of
    the constructors of an expression.                                                       -}


-- ============
-- 5. Examples.
-- ============

--  We can now reap the rewards of our investment..
-- How hard is it to add multiplication to our expression language?
data Mul e = Mul e e deriving (Eq,Show)
instance Functor Mul where
    fmap f (Mul x y) = Mul (f x) (f y)
instance Eval Mul where
    evalAlgebra (Mul x y) = x * y
infixl 7 <*>
(<*>) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x <*> y = inject (Mul x y)
-- we can now evaluate expressions with multiplication...
mulExample :: Expr (Val :+: Add :+: Mul) = val 80 <*> val 5 <+> val 4
mulExampleVal = eval mulExample
mulExample2 :: Expr (Val :+: Mul) = val 6 <*> val 7
mulExample2Val = eval mulExample2

{-  As mulExample2 demonstrates, we can also write and evaluate expressions of type 
    Expr (Val :+: Mul), leaving out addition.
    Once we have a menu of expression building blocks, we can assemble our own datatypes
    a la carte. This isn't even possible with the proposed language extension for open data
    types (Loh & Hinze, "Open data types and open functions").                            -}
{-  Adding new functions isn't hard. As an example, we show how to render an expression as a
    string. Rather than writing this as a fold, we show how to write an open-ended function
    using recursion directly                                                              -}
-- we begin by defining a class corresponding to the function we wish to write...
class Render f where
    render :: Render g => f (Expr g) -> String
{-  Note that the type   render :: f (Expr f) -> String   is not general enough. For example,
    the instance definition for Add would require that all recursively-called subexpressions
     consist entirely of additions. However, we would like subtrees to be of any kind of 
    expression (that supports render)                                                      -}
-- Assuming we have defined necessary instances, we can use Render to write a pretty-printer
pretty :: Render f => Expr f -> String
pretty (In t) = render t
-- instance definitions...
instance (Render f, Render g) => Render (f :+: g) where
    render (Inl x) = render x
    render (Inr y) = render y
instance Render Val where
    render (Val n) = show n
instance Render Add where
    render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
instance Render Mul where
    render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"
-- we can now pretty-print expressions...
prettyExample = pretty mulExample

{-  Note: The inj function of the (:<:) class has a partial inverse.
          We add to the class the second function...
    prj :: sup a -> Maybe sub a
          prj is particularly useful when performing complex pattern matches on expressions.

    For example, we may wish to rewrite expressions, distributing multiplication over addition.
    To do so, we need to know whether one of the children of a Mul constructor is an Add.
    Using the Maybe monad and prj function, we may attempt to apply the distributive law on the
      outermost constructors of an expression...                                              -}
match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In t) = prj t  -- (In t) is type Expr f, so t is type f(Expr f) and (prj t) is g(Expr f)
distr :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distr t = do Mul a b <- match t
             Add c d <- match b
             return (a<*>c <+> a<*>d)
{-  Using distr, we can define an algebra to fold over an expression, applying distributivity
    uniformly whenever possible, rather than just inspecting the outermost constructor.     -}

{-  We've seen how we can add both new constructors and new functions to our types, without
    needing to modify existing code.
    In fact, this approach is not limited to datatypes: we can use the same techniques to
    combine a certain class of monads...                                                -}


-- ===================
-- 6. Monads for Free.
-- ===================

{-  Modern calculators, beyond their various numerical functions, have memory cells. Haskell
    encapsulates such mutable state using monads. However, despite all the virtues of monads,
    they are notoriously difficult to combine.
    Can we extend our approach to monads?

    In general, the coproduct of two monads is quite complicated.
    (See Luth & Ghani, "Composing monads using coproducts").
    To simplify things, we will restrict ourselves to "free monads", ie. monads of the form.. -}
data Term f a = Pure a
              | Impure (f (Term f a))
-- Often, we write Free<-Term, Return<-Pure and Roll<-Inpure
-- Term takes a functor and folds it recursively upon either a naked value, or itself.
-- These monads consist of either pure values, or an impure effect constructed using f
-- When f is a functor, (Term f) is a monad...
instance Functor f => Functor (Term f) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Impure t) = Impure (fmap (fmap f) t)
instance Functor f => Monad (Term f) where
    return x = Pure x
    (>>=) (Pure x) f = f x
    (>>=) (Impure t) f = Impure (fmap (>>=f) t)
-- when we bind (Impure t), we substitute subtrees for all the naked variables in our monad.
-- see a text on Category Theory for more on Free Monads
-- also, The Comonad Reader, "Monads for Free".
-- Seversl familiar monads are actually free monads.

-- Consider the following types...
data Zero a                -- requires EmptyDataDecls
data One a = One
data Const e a = Const e
{-  Now, (Term Zero) is the identity monad, (Term One) corresponds to the Maybe monad and
    (Term (Const e)) is the error monad.
    However, most monads are *not* free, including the list monad and the state monad.    -}

{-  In general, a structure is "free" if it is left-adjoint to a forgetful functor.
    In this instance, the Term datatype is a higher-order functor mapping a functor f to
      a monad (Term f), as illustrated by the instance defintions above.
    This Term functor is left-adjoint to the forgetful functor from monads to their underlying
      functors.                                                                             -}

{-  All left-adjoint functors preserve coproducts.
    In particular, computing the coproduct of two free monads reduces to computing the 
      coproduct of their underlying functors. This is the property we used in section 2, and
      in this section we will exploit it to define monads modularly.                       -}

{-  Although the state monad is not free, we can use the Term datatype to represent a language
    of stateful computation. Let's imagine a simple calculator equipped with 3 buttons for 
    modifying/accessing the memory:
      recall: returns the value currently stored in memory;
      increment: (M+) adds a number ot the value in memory. We'll call this "Incr"
      clear: resets the memory to zero                                                    -}
{-  Again, we define types Incr and Recall corresponding to the operations we wish to
    introduce. The Incr constructor takes two arguments: the integer with which to increment
    the memory cel, and the remainder of the computation. The Recall constructor takes a single
    argument: a function which expects to receive the contents of the cell, given which it
    continues the rest of the computation.
    Both of these types are obviously functors...                                          -}
data Incr t = Incr Int t
data Recall t = Recall (Int -> t)
-- to facilitate writing these terms, we define another series of smart constructors...
inject' :: (g :<: f) => g (Term f a) -> Term f a
inject' = Impure . inj
incr :: (Incr :<: f) => Int -> Term f ()
incr i = inject' (Incr i (Pure ()))
recall :: (Recall :<: f) => Term f Int
recall = inject' (Recall Pure)

             -- CONTINUE LATER --
