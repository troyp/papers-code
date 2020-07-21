{-# LANGUAGE TypeSynonymInstances #-}
-- *************************************
--                                     *
-- * MONADS FOR FUNCTIONAL PROGRAMMING *
-- *          Philip Wadler            *
-- *                                   *
-- *************************************

-- ==========================
-- Part 2: EVALUATING MONADS.
-- ==========================

import Control.Monad.Identity
-- variation zero: the basic interpreter
data Term = Con Int
          | Div Term Term
          deriving (Show, Eq)
eval0 :: Term -> Int
eval0 (Con a) = a
eval0 (Div t u) = (eval0 t) `div` (eval0 u)

answer, err :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
err = (Div (Con 1) (Con 0))

--variation one: exceptions
data M1 a = Raise Exception
         | Return a
         deriving (Show, Eq)
type Exception = String
eval1 :: Term -> M1 Int
eval1 (Con a) = Return a
eval1 (Div t u) =
    case eval1 t of
      Raise e -> Raise e
      Return a ->
          case eval1 u of
            Raise e -> Raise e
            Return b -> if b==0 then Raise "division by zero"
                        else Return (a`div`b)

-- variation two: state
-- suppose we wish to count the number of divisions performed during evaluation
type M2 a = State -> (a, State)
type State = Int
eval2 :: Term -> M2 Int
eval2 (Con a) x = (a, x)
eval2 (Div t u) x =
    let (a, y) = eval2 t x in
    let (b, z) = eval2 u y in
    (a`div`b, z+1)

-- variation three: output
-- suppose we wish to display an execution trace
type M3 a = (Output, a)
type Output = String
eval3 :: Term -> M3 Int
eval3 (Con a) = (line (Con a) a, a)
eval3 (Div t u) =
    let (x, a) = eval3 t in
    let (y, b) = eval3 u in
    (x ++ y ++ line (Div t u) (a`div`b), a`div`b)
line :: Term -> Int -> Output
line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ "\n"
 
-- A monadic Evaluator.
{-  Each of the evaluators has a similar structure, which may be abstracted to
    yield the concept of a monad. A function of type a->b is replaced with one
    of type a->M b, which takes an arg of type a and returns a result of type b,
    with a possible additional effect captured by M.
    What sort of operations are required on type M?
    1. We need a way to turn a value into the computation that returns that
       value and does nothing else.
       unit :: a -> M a
    2. We need a way to apply a function of type a -> M b to a computation of
       type M a. It is convenient to write the arg before the function...
       (*) :: M a -> (a -> M b) -> M b
    A monad is a triple (M, unit, *) consosting of a type constructor M and 
    two operations of the given polymorphic types. These operations must obey
    the "monad laws" given below.

    We will often write expressions of the form
    m * \a->n    where m,n are expressions and a is a variable
    This can be read "perform computation m, bind a to the result, then perform
    computation n (with bound variable a), passing this value in.
    do a<-m ; \a->n                -}
class Monad' m where
    unit :: a -> m a
    (>>>=) :: m a -> (a -> m b) -> m b
eval :: Monad' m => Term -> m Int
eval (Con a) = unit a
eval (Div t u) = eval t >>>= \a
               -> eval u >>>= \b
               -> unit (a`div`b)
-- remember: lambda abstraction has weakest binding, application has strongest
-- eval is a bit more complicated than eval0, but much more foexible...

-- variation zero revisited: the basic evaluator
newtype M0 a = M0 {runIdt :: a}    -- isentity monad
    deriving (Show, Eq)
unit0 :: a -> M0 a
unit0 a = (M0 a)
bind0 :: M0 a -> (a -> M0 b) -> M0 b
m `bind0` k = k (runIdt m)
instance Monad' M0 where
    unit = unit0
    (>>>=) = bind0
{- This is the identity monad: M0 is the identity funtion
   on types, unit0 is the identity function and bind0 is
   just function application.                -}

-- variation one revisited: exceptions
data M1' a = Raise' Exception' | Return' a
           deriving (Show, Eq)
type Exception' = String
unit1 :: a -> M1' a
unit1 a = Return' a
bind1 :: M1' a -> (a -> M1' b) -> M1' b
m`bind1`k = case m of
              Raise' e -> Raise' e
              Return' a -> k a
raise' :: Exception' -> M1' a
raise' e = Raise' e
instance Monad' M1' where
    unit = unit1
    (>>>=) = bind1
-- to add error handling to the monadic evaluator...
eval_eh :: Term -> M1' Int
eval_eh (Con a) = unit a
eval_eh (Div t u) = eval t >>>= \a
                  -> eval u >>>= \b
                  -> if b==0
                     then raise' "divide by zero"
                     else unit (a`div`b)

-- variation two revisited: state
