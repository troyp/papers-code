module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

-- We will create an interpreter for a simple programming language.
type Name = String                 -- variable names
data Exp = Lit  Integer            -- expressions
         | Var  Name
         | Plus Exp Exp
         | Abs  Name Exp
         | App  Exp Exp
         deriving (Show)
{-  Expression datatype has variants for literal integers (constants),
    variables, addition, lambda expressions (abstractions) and function
    application.                -}
data Value = IntVal Integer        -- values
           | FunVal Env Name Exp
           deriving (Show)
type Env = Map.Map Name Value      -- mapping from names to values
{-  The programs to be evaluated will be of type Exp and results of type Value.
    The Env component of a FunVal is the environment in which the corresponding
    lambda abstraction is evaluated,                -}

{-  We now define a (non-monadic) interpreter eval0 for this language, as a
     "reference implementation".                -}
eval0 :: Env -> Exp -> Value
eval0 _   (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
    let val1 = eval0 env e1
        val2 = eval0 env e2
    in case val1 of
         FunVal env' n body -> eval0 (Map.insert n val2 env') body

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleResult0 = eval0 Map.empty exampleExp


-- ----------------------------
-- Converting to Monadic Style.
-- ----------------------------

{-  To use Monad Transformers, we must express our functions in monadic style.
    We will use do notation to impose sequencing and return to specify results.
    We must choose a monad in which the evaluator will be defined. We will use
    the Identity monad as our base                -}
type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 = runIdentity

--mLookup replaces old version of Data.Map.lookup.
mLookup :: (Ord k, Monad m)=> k -> Map.Map k a -> m a
mLookup n datamap = let val = Map.lookup n datamap
                    in case val of
                         Just x -> return x
                         Nothing -> fail "Data.Map.lookup: Key not found"
--return (fromJust val)

eval1 :: Env -> Exp -> Eval1 Value
eval1 _   (Lit i) = return $ IntVal i
eval1 env (Var n) = mLookup n env
eval1 env (Plus e1 e2) = do
  IntVal i1 <- eval1 env e1
  IntVal i2 <- eval1 env e2
  return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body -> eval1 (Map.insert n val2 env') body

exampleResult1 = runEval1 $ eval1 Map.empty exampleExp

-- ----------------------
-- Adding Error Handling.
-- ----------------------

type Eval2 a = ErrorT String Identity a
-- String is the type of the exceptions - ie. we use simple string messages.
{-  The function for running a computation in this eval2 monad changes in 2
    ways from the eval1 case...
    1. The result is now (Either String a) where
        Left s   indicates that an error has occured with message s,
        Right r  indicates succesful evaluation with result r.
    2. We need to call runErrorT on the computation to obtain Identity, then
       call runIdentity to complete evaluation.                -}
runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runErrorT

-- It's simple to convert eval1 to type Eval2...
eval2a :: Env -> Exp -> Eval2 Value
eval2a _   (Lit i) = return $ IntVal i
eval2a env (Var n) = mLookup n env
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do
  val1 <- eval2a env e1
  val2 <- eval2a env e2
  case val1 of
    FunVal env' n body -> eval2a (Map.insert n val2 env') body

exampleResult2a = runEval2 $ eval2a Map.empty exampleExp

{-  However this does not actually use the error reporting of the ErrorT 
    transformer. It just wraps the result of valid expressions in a Right
    constructor. We must modify our definition to use error reporting..    -}

eval2b :: Env -> Exp -> Eval2 Value
eval2b _   (Lit i) = return $ IntVal i
eval2b env (Var n) = mLookup n env
eval2b env (Plus e1 e2) = do
  e1' <- eval2b env e1
  e2' <- eval2b env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do
  val1 <- eval2b env e1
  val2 <- eval2b env e2
  case val1 of
    FunVal env' n body -> eval2b (Map.insert n val2 env') body
    _ -> throwError "type error"

exampleResult2b = runEval2 $ eval2b Map.empty exampleExp
exampleResult2bfail = runEval2 $ eval2b Map.empty 
                      (Plus (Lit 1) (Abs "x" (Var "x")))

{-  Note that monadic binding in a do-expression uses the fail function
    whenever a pattern match fails. We can exploit this fact to create a
    shorter version of eval2...                -}
eval2c :: Env -> Exp -> Eval2 Value
eval2c _   (Lit i) = return $ IntVal i
eval2c env (Var n) = mLookup n env
eval2c env (Plus e1 e2) = do
  IntVal i1 <- eval2c env e1
  IntVal i2 <- eval2c env e2
  return $ IntVal (i1 + i2)
eval2c env (Abs n e) = return $ FunVal env n e
eval2c env (App e1 e2) = do
  FunVal env' n body <- eval2c env e1
  val2               <- eval2c env e2
  eval2c (Map.insert n val2 env') body

exampleResult2c = runEval2 $ eval2c Map.empty exampleExp
exampleResult2cfail = runEval2 $ eval2c Map.empty 
                      (Plus (Lit 1) (Abs "x" (Var "x")))

{-  However, the problem with this approach is that the error messages only
    mentions "pattern match failure" without any more specific information.
    In order to get the most useful error messages, it is best to provide our
    own calls to throwError...                -}

-- final version of eval2...
eval2 :: Env -> Exp -> Eval2 Value
eval2 _   (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                      Nothing  -> throwError ("unbound variable: " ++ n)
                      Just val -> return val
eval2 env (Plus e1 e2) = do
  e1' <- eval2 env e1
  e2' <- eval2 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
  val1 <- eval2 env e1
  val2 <- eval2 env e2
  case val1 of
    FunVal env' n body -> eval2 (Map.insert n val2 env') body
    _ -> throwError "type error in application"

exampleResult2 = runEval2 $ eval2 Map.empty exampleExp
exampleResult2fail = runEval2 $ eval2 Map.empty 
                      (Plus (Lit 1) (Abs "x" (Var "x")))


-- -----------------------
-- Hiding the environment.
-- -----------------------

{-  The environment is only modified in one place (funciton application) and
    used in two places (variables and lambda abstraction). We can reduce the
    amount of code by hiding it in all other places.
    We will use a ReaderT monad transformer to implement a reader monad.
    A reader monad passes a value into a computation and all of its
    subcomputations. This value can be read by all enclosed computations and
    can be modified in nested computations. An encapsulated computation cannot
    change the value used by surrounding computations (this is in contrast to
    the state monads).                -}
type Eval3 a = ReaderT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runErrorT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                     Nothing  -> throwError ("unbound variavle: " ++ n)
                     Just val -> return val
eval3 (Plus e1 e2) = do
  e1' <- eval3 e1
  e2' <- eval3 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return $ FunVal env n e
eval3 (App e1 e2) = do
  val1 <- eval3 e1
  val2 <- eval3 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
    _ -> throwError "ype error in application"

exampleResult3 = runEval3 Map.empty  (eval3 exampleExp)

{-  When the current environment is needed, it is extracted from the reader
    monad with the function "ask". For function application, the function
    "local" is used to modify the environment for the recursive call. "local"
    has type (r->r) -> m a -< m a, ie. the first arg is a function that maps
    the current environment to the new local environment. In our case, the
    nested environment doesn't depend on the current environment (except insofar
    as it has already affected the evaluation of e1), so we just create a
    constant function by passing the nested env value to "const".    -}


-- -------------
-- Adding state.
-- -------------

{-  An important application of monads is adding mutable state to otherwise
    purely functional code. We will add profiling capabilities to our
    interpreter by incorporating state (we will use a simple integer value).
    We define a new monad by wrapping a StateT constructor around the innermost
    monad (in the case of State and Error monads, the order of construction is
    important).                -}
type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

-- we will count the number of eval steps, ie. the number of calls to eval4
-- helper function "tick" gets the state, increments and stores it back.
-- we define a polymorphic version of tick so it can be reused.
tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do tick
                   return $ IntVal i
eval4 (Var n) = do tick
                   env <- ask
                   case Map.lookup n env of
                     Nothing  -> throwError ("unbound variable: " ++ n)
                     Just val -> return val
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval4 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval4 (App e1 e2) = do
  tick
  val1 <- eval4 e1
  val2 <- eval4 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body)
    _ -> throwError "type error: application"

exampleResult4 = runEval4 Map.empty 0 (eval4 exampleExp)

{-  Note: when StateT and ErrorT are swapped in the type of eval4, the
    interpretation changes. Instead of returning a result (error or normal)
    together with a state, the new monad returns either a normal result with
    a state, or an error. The positioning of the Reader monad doesn't matter,
    since it doesn't contribute to the final value.                -}
type Eval4' a = ReaderT Env (StateT Integer (ErrorT String Identity)) a
runEval4' :: Env -> Integer -> Eval4' a -> Either String (a, Integer)
runEval4' env st ev = runIdentity (runErrorT (runStateT (runReaderT ev env) st))
-- eval4' not implemented


-- ---------------
-- Adding Logging.
-- ---------------

{-  Finally, we use transformer WriterT. Whereas ReaderT lets you use values
    passed in in a computation, WriterT lets you add values to the result of a
    computation. Note that like StateT, WriterT produces output and therefore
    interacts with ErrorT. Depending on the order of WriterT and ErrorT, an
    error result will or will not include the values written.    -}
type Eval5 a = ReaderT Env (ErrorT String
                            (WriterT [String] (StateT Integer Identity))) a
{-  The values to be written out will be lists of strings. Note that the type
    of the output values for WriterT is restricted to instances of Monoid -}
runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev =
    runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)
{-  We'll illustrate the use of WriterT by writing out the names of each
    variable encountered                -}

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do tick
                   return $ IntVal i
eval5 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case Map.lookup n env of
                     Nothing  -> throwError ("unbound variable: " ++ n)
                     Just val -> return val
eval5 (Plus e1 e2) = do tick
                        e1' <- eval5 e1
                        e2' <- eval5 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval5 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval5 (App e1 e2) = do
  tick
  val1 <- eval5 e1
  val2 <- eval5 e2
  case val1 of
    FunVal env' n body ->
        local (const (Map.insert n val2 env'))
              (eval5 body)
    _ -> throwError "type error in application"

exampleResult5 = runEval5 Map.empty 0 (eval5 exampleExp)


-- ---------------
-- Integrating IO.
-- ---------------

{-  It's not possible to define an IO monad transformer, since execution of IO
    operations cannot be arbitrarily nested into other functions or monads -
    They are only allowed in the IO monad.
    How can we integrate IO into our definitions?
    We simply replace Identity with IO.
    This is possible because Identity is the base monad (as we've seen,
    runIdentity is always applied last).                -}
type Eval6 a = ReaderT Env (ErrorT String
                            (WriterT [String] (StateT Integer IO))) a
{-  The return type of runEval6 is wrapped in an IO constructor, which means
    running an Eval6 computation does not directly return a result, but rather
    an IO computation which must be run to get at the result.
    Hence, the runIdentity invocation disappears and is not replaced.    -}
runEval6 :: Env -> Integer -> Eval6 a ->
            IO ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st

{-  We can now use IO operations in our definition of eval6, but must use liftIO
    to lift them inot the current monad.                -}
-- example: we'll print out each integer constant as soon as it's evaluated
eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do tick
                   liftIO $ print i
                   return $ IntVal i
eval6 (Var n) = do tick
                   env <- ask
                   case (Map.lookup n env) of
                     Nothing -> throwError ("unbound varaible: " ++ n)
                     Just val -> return val
eval6 (Plus e1 e2) = do tick
                        e1' <- eval6 e1
                        e2' <- eval6 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval6 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval6 (App e1 e2) = do
  tick
  val1 <- eval6 e1
  val2 <- eval6 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
    _ -> throwError "type error in application"

exampleResult6 = runEval6 Map.empty 0 (eval6 exampleExp)
