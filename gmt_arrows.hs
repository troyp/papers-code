import Prelude hiding ((++))
import Control.Monad
import Control.Applicative (Alternative(..))
import Data.Maybe

-- ***********************************
-- *                                 *
-- *  GENERALISING MONADS TO ARROWS  *
-- *           John Hughes.          *
-- *                                 *
-- ***********************************


-- ================
-- 1. Introduction.
-- ================
{-  One of the distinguishing features of FP is the use of *combinators* to construct programs.
    A combinator is a function that builds program fragments from program fragments:
    a programmer using combinators constructs much of the program automatically.
    Some combinators, like map and filter, encapsulate broadly useful constructions and may be
    found in almost any functional program. Others are applicable to specific application areas
    and are collected into libraries which allow easy and rapid application development. For
    example, parsing is an area which has been extensively studied, and several combinator
    libraries are available for constructing parsers. A parser for the grammar  G ::= aGb | c
    might be programmed in Haskell as
gram = symbol "a" `cat` gram `cat` symbol "b" +++ symbol "c"
    Although combinator programming is an old idea, it has been profoundly influenced recently
    by Wadler's introduction of monads into FP. We can consider a monad to be a kind of
    standardised interface to an abstract data type of 'program fragments'. The monadic
    interface is suitable for many combinator libraries and is now widely used. There are many
    benefits to a standardised interface. For example, Haskell has been extended with special
    constructors to make the use of monads more convenient.
    Unfortunately, there are some libraries which, for fundamental reasons, cannot use the
    monadic interface. In particular, Swierstra & Duponcheel have developed a lib for parsing
    LL1 grammars that avoids a well-known inefficiency in monadic parsing libs by combining the
    construction of a parser with a static analysis of the program being constructed. Yet this
    optimisation is incompatible with the monadic inteface.
    Swierstra & Duponcheel's library demonstrates a generally useful paradigm for combinator
    design. So inspired, we discuss a generealisation of monads called "arrows" which is
    applicable to this parsing library as well as other nonmonadic libaries such as the fudgets
    GUI library.
    Although arrow are less convenient than monads, they are more widely applicable and can
    bring some of the benefits of monads to a wider class of applications.                 -}


-- ===========================================
-- 2. Background: Library Design using Monads.
-- ===========================================
-- In Haskell, the monadic interface is defined as a class...
class Monad' m where
  return' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b
{-  Intuitively, we think of a monadic value of type (m a) as representing a *computation*
    with result of type a - a program fragment. "return" constructs a trivial computation
    with its arg as the result. "bind" combines two computations with the first passing its
    result to the second as an arg - thus the second arg of "bind" is a function that
    constructs the second computation, rather than simply a computation.                -}

-- Example: A monad to manage failures.
-- ------------------------------------
data Maybe' a = Just' a | Nothing'  deriving (Show, Eq)
-- This type can be more conveniently used if we have a combinator library to handle failure...
instance Monad' Maybe' where
  return' a = Just' a
  x`bind'`f = case x of
                Just' a  -> f a
                Nothing' -> Nothing'
{-  Using the monad combinators, we can write functions which handles failure without any
    explicit testing for Just and Nothing...                                            -}
-- add 2 possibly failing integers, failing if either arg does...
madd :: Maybe Int -> Maybe Int -> Maybe Int
madd x y = x >>= \a ->
                   y >>= \b ->
          return (a+b)
-- a useful library for failure handling needs a combinator to cause failure...
fail :: Maybe a
fail = Nothing
{-  Now we can treat the Maybe type as abstract, and write programs that cause and propagate
    failures just using the operators fail, return and bind., with no explicit dependence on
    the way failures are represented.                                                      -}

-- Another example: A monad to manage state.
-- -----------------------------------------
{-  Mutable state can be modelled in a Purely FL by passing the state to each function as an
    additional arg and returning the (possibly modified) state as part of each result. Doing
    this by hand is tedious and error-prone, but luckily we can encapsulate the state-passing
    mechanism in a combinator library by using a monad.                                     -}
-- we represent a computation with result type a and state type s, by a value of type...
newtype StateMonad s a = SM { smFunc :: s->(a,s) }
-- The partially-applied type (StateMonad s) denotes a parametrised type with 1 additional param
-- For any state type s, (StateMonad s) is a monad
instance Functor (StateMonad s) where
  fmap = liftM
instance Applicative (StateMonad s) where
  pure = return
  (<*>) = ap
instance Monad (StateMonad s) where
  return a = SM (\s -> (a,s))
  x >>= f  = SM (\s -> let x' = smFunc x
                           (a,s') = x' s
                           f' = smFunc (f a)
                       in f' s')
{-  original (without smFunc..
    x >>= f  = SM (\s -> let SM x' = x
                             (a,s') = x' s
                             SM f' = f a
                             (b,s'') = f' s'
                         in (b,s''))                                                -}
{-  With these defns, we can write a program that passes around a state simply in terms of
    return and bind - there's no need to manipulate state explicitly.
    To complete a state passing lib, we must provide combinators for reading and modifying
    a state...                                                                           -}
fetch :: StateMonad s s
fetch = SM (\s -> (s,s))
store :: s -> StateMonad s ()
store x = SM (\s -> ((), x))
-- Now, StateMonad can be made abstract, and stateful progs written just in terms of combinators
-- For example, a function to increment a counter...
tick :: StateMonad Int Int
tick = fetch >>= \n ->
       store (n+1) >>= \() ->
       return n

-- Monadic parsing combinators.
-- ----------------------------
-- In practice, combinator libs are based on monads providing a combination of features.
-- For example, a parser for values of type a represented using symbols of type s might be
newtype Parser' s a = P' { parserFunc :: [s] -> Maybe (a,[s]) }
{-  We invokde a parser by applying its representation to a list of symbols to parse: its
    result indicates whether or not parsing was successful and, if so, contains both the
    value parsed and the remaining unparsed input.                                        -}
-- a parser which recognises a particular symbol...
symbol' :: Eq s=> s -> Parser' s s
symbol' s = P' (\xs -> case xs of
                       [] -> Nothing
                       (x:xs') -> if x==s then Just (s,xs') else Nothing)
-- fails if input is empty or begins with wrong symbol; else, succeeds with one symbol consumed
{-  This representation of parsers supports a combination of failure handling and state
    passing, where the state is the unparsed input. It can be declared as a monad just
    like the other types above. See Wadler's papers for further details. By adding more
    combinators, we can build a complete parsing lib based on this monad.             -}
instance Functor (Parser' s ) where
    fmap = liftM
instance Applicative (Parser' s) where
    pure = return
    (<*>) = ap
instance Monad (Parser' s) where
    return a = P' (\cs -> Just (a,cs))
    x >>= f  = P' (\cs -> let x_fn = parserFunc x
                              res = x_fn cs
                          in case res of
                              Nothing -> Nothing
                              Just (a,cs') -> (parserFunc (f a)) cs)
-- Why use monads?
-- ---------------
{-  Use of any combinator library simplifies code dramatically, but why use monads in
    particular?
    * monads, being a 'standard' interface, offer a design guidline for combinator libs:
      it's often a good start to design a suitable monad. For ex, a parsing lib needs a
      combinator to invoke two parsers in sequence, but there are many ways to implement
      this. In some early libs, the two results were paired together, others used an extra
      param for a function to combine the results. The monadic bind op is more general
      than either of these: both may be defined in terms of bind, but the converse isn't
      true. Using a monadic interface is likely to give superior results to such ad hoc
      alternatives - we know from experience that it provides great power.
    * Monads also offer flexibililty: there are many possible implementations. Indeed, we
      can use monad transformers to systematically construct suitable monads. The interface
      exposes very little of the lib's internal workings, which helps make the lib future-
      proof. Since the monadic interface doesn't constrain the choice of monad very much,
      we can modify a lib without forcing changes on client code.
    * Since return and (>>=) are overloaded in Haskell, we can write *generic* monadic code,
      which can be used with any monadic lib. There are many such monadic functions in the
      Haskell std libs. For example, a generalisation of the "add" function above is liftM2,
      which applies *any* binary operator to the results of two computations. We can use
      this to define the function cat from the introduction, as simply cat = liftM2 (++).
      These generic functions provide free functionality to the library author, making the
      development of libs much easier.                                                   -}

-- Further parsing combinators.
-- ----------------------------
{-  One of the things a parser can do is to fail. To express this, we define a combinator
    that always fails. In fact, many monads support a notion of failure, so it's useful to
    overload the failure operator, just like we did with return and (>>=). We do this using
    a type class...                                                                       -}
class Monad m => MonadZero m where
    zero' :: m a
-- ie. a parametrised type m is a MonadZero if it is a Monad, and also supports the zero op
instance MonadZero (Parser' s) where
    zero' = P' (\s -> Nothing)
{-  Many monads which support failure also support a choice combinator, which tries two ways
    to perform a computation, using the second if the first one fails.                     -}
class MonadZero m => MonadPlus' m where
    plus' :: m a -> m a -> m a
{-  We'll use the Control.Monad version which lumps failure and choice together. We'll hide
    the Prelude's ++ and use it for the more general mplus (we need to make sure we define
    [] as MonadPlus so we can still use ++ normally on lists, but Control.Monad does this.  -}
zero :: MonadPlus m => m a
zero = mzero
(++) :: MonadPlus m => m a -> m a -> m a
(++) = mplus
--
-- MonadPlus implementation for Parser...
instance Alternative (Parser' s) where
  (<|>) = mplus
  empty = mzero
instance MonadPlus (Parser' s) where
    mzero = P' (\s -> Nothing)
    P' a `mplus` P' b = P' (\s -> case a s of
                                 Just (x,s') -> Just (x,s')
                                 Nothing -> b s)
{-  This is one of the fundamental building blocks of a parsing lib: all interesting
    grammars define some nontermianls using alternatives.
    Unfortunately, this implementation contains a serious *space leak*. That is, it causes
    the GC to retain data much longer than would be naively anticipated, and hence parsers
    built with this op use much more space than you'd reasonably expect.
    The problem is actually inherent to backtracking parsers: the input to be parsed, s,
    can't be GC'd while the first parser, a, is running, since we might need to pass s to
    b if a fails. In a lazy language, the act of running parser a forces the list of tokens
    s to be constructed. If a fails quickly, little space is used, but if a succeeds in
    parsing a large part of s, then a great deal of space is used to hold these already-
    parsed tokens in case a eventually fails. (Ironically, if a proceeds a long way before
    failing, b will probably fail too, so the expense is likely to be wasted, anyway).
    This problem has been known since the proposal of parsing combinator libraries. Wadler
    described a partial solution in "How to replace failure with a list of successes". It
    relies on a special combinator like Prolog's "cut", to declare that the parser need not
    backtrack past a certain point.
    Monadic parser libs work well in practice, but the fundamental problem remains.      -}


-- ============================================
-- 3. Swierstra & Duponcheel's Parsing Library.
-- ============================================
{-  A different solution was presented in Swierstra & Duponcheel, "Deterministic, Error-
    Correcting Combinator Parsers", 1996. They restrict their attention to LL1 parsers, in
    which alternative parses can be decided by looking at the next token. Their
    implementation of a++b can thus be decided immediately, with no need to save the input.
    Thus, the space leak is avoided.
    To implement this, we must be able to tell, given a parser, which tokens it might accept
    as the start of input (and also, whether it can accept the empty sequence of tokens).
    This means parsers can no longer be represented as functions, as they were above. Instead,
    they are represented as a combination of static information (which can be computed before
    parsing begins) and a parsing function, which can be optimised based on the static info. -}
data StaticParser s = SP Bool [s]
newtype DynamicParser s a = DP ([s] -> (a,[s]))
data Parser s a = P (StaticParser s) (DynamicParser s a)
{-  The first, static, component tells us whether the parser matches the empty string, and
    gives a list of valid start tokens.
    The second, dynamic, component, is a function that does the actual parsing.            -}
-- The combinator which accepts a particular symbol...
symbol :: s -> Parser s s
symbol s = P (SP False [s]) (DP (\(x:xs) -> (s,xs)))
{-  The dynamic parsing function need not test for empty input or check that the first symbol
    is s, because it will only be invoked when the preconditions expressed by the static part
    are met.
    We can make use of static info to define the choice combinator efficiently              -}
-- we use choice rather than (++) or mplus, since (Parser s) is not a monad (as we'll see)
choice :: Eq s => Parser s a -> Parser s a -> Parser s a
P (SP e1 init1) (DP p1) `choice` P (SP e2 init2) (DP p2) =
    P (SP (e1 || e2) (init1 ++ init2))
      (DP (\xs -> case xs of
                    [] -> if e1 then p1 [] else p2 []
                    x:xs' -> if x`elem`init1 then p1 (x:xs') else
                                 if x`elem`init2 then p2 (x:xs') else
                                     if e1 then p1 (x:xs') else p2 (x:xs')))
{-  The choice of whether to invoke p1 or p2 is made directly, and cannot be revised, so
    there's no need to retain a pointer to the input (and hence, no space leak).       -}
{-  Just as "choice" computes the starter symbols and potential emptiness of the parser it
    constructs, so must all the other combinators. In most cases, this is straightforward,
    but unfortunately, it turns out to be impossible for >>=.
    Consider the type >>= would have in this case...
(>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    Now the static properties of the result would depend on the static properties of both
    the first and second arg - eg. the combination can match the empty sequence if and only
    if both args can. Now, in order to get the static properties of the second arg, we must
    apply it to a value of type ::a. Such values will only be constructed during parsing,
    but for this to be useful, we want to compute the static parts of parsers once and for
    all, before parsing begins. There is no defn of >>= that does this.                  -}
{-  Swierstra & Duponcheel's solution is to abandon the monadic interface: rather than >>=,
    they defined a different sequencing operator ...
    (<*>) :: Parser s (a->b) -> Parser s a -> Parser s b
    This is perfectly adequate for expressing parsers, and does not pose problems with
    computing static properties in advance of parsing. However, the need to abandon the
    monad signature means the parsing lib stands alone: it cannot, for example, be used
    with generic monadic fuctions.
    If this were an isolated case, we might ignore it, but Swierstra and Duponcheel's idea
    of defining a domain specific combinator library using a combination of static analysis
    and optimised dynamic analysis is widely applicable. We may wish to do this often, but
    every time we do, the type of >>= will make it impossible to use a monadic interface.
    Thus, we would like to find a generalisation of monads which can serve as a generic
    interface for a much wider range of combinator libraries.                           -}



-- ==========
-- 4. Arrows.
-- ==========
{-  Recall that we are unable to implement bind for Swierstra & Duponcheel's parsers..
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    since its second arg is a function, and all we can do with a function is to apply it.
    Lacking any suitable value of type a to apply it to, we can't extract any static info
    from it, and thus can't construct the static part of >>='s result.
    Our solution is simply to *change the representation* of this arg. Rather than a
    function ::a -> Parser b, we'll use an abstract type, which we call an "arrow" from
    a to b. We solve the problem byu choosing a representation which makes static
    properties immendiately accessible.
    Rather than deal with 2 abstract types (monad and arrow), we'll work purely with arrows.

    In general, an arrow type will be a parametrised type with two parameters, supporting
    ops analogous to return and bind.
    Just as we think of a monad (m a) as "a computation delivering an a",
    we think of an arrow type (a b c), ie. the application of parametrised type a to the two
    parameters b and c, as "a computation with input of type b, delivering a c".
    Arrows make the dependence on input explicit.
    Just as there's a Monad class, so we'll define an Arrow class with analogous operators.
    But we must make the dependence on input explicit.
    Thus, while return :: a -> m a, merely converts a value into a computation, its analogue
    for arrows, arr :: (b->c) -> a b c, converts a function from b to c, into a computation.
    The analogue of >>= is just composition of arrows.                                    -}
class Arrow a where
    arr :: (b -> c) -> a b c
    (>>>) :: a b c -> a c d -> a b d
    first :: a b c -> a (b,d) (c,d)
-- For any monad m, functions ::a -> m b, are potential arrows. If we give this type a name..
newtype Kleisli m a b = K (a -> m b)
-- then we can implement the arrow ops as follows...
instance Monad m => Arrow (Kleisli m) where
    arr f = K (return . f)
    K f >>> K g = K (\b -> f b >>= g)
    first (K f) = K (\(b,d) -> f b >>= \c -> return (c,d))
{-  So, for every monad type, there is a corresponding arrow type. Categorically speaking,
    we construct the Kleisli Category of the monad m. Of course, there are also many other,
    nonmonadic, implementations of the arrow signature.                                   -}
instance Arrow (->) where
    arr = id
    f >>> g = g . f
    first f (x,y) = (f x, y)

-- Arrows and pairs.
-- -----------------
{-  Although monadic return and bind are enough to begin writing useful code, the same is
    not true for the analogous ops for arrows. Even the simple monadic addition function
    we saw earlier cannot yet be expressed in an arrow form..
add :: Monad m => m Int -> m Int -> m Int
add x y = x >>= \u -> y >>= \v -> return (u+v)
    making dependence on input explicit, an analogous defn should take the form...
add :: Arrow a => a b Int -> a b Int -> a b Int
add f g = ...
    where we must combine f and g in sequence. Our only sequencing op so far is >>>, but
    f and g don't have the right types to be composed. Indeed, "add" needs to save the
    input of type b across the computation of f, in order to supply the same input to g.
    Also, it must save the result of f across the computation of g, so the two results
    can be combined. So far, we have no way to save a value across another computation,
    so we need another combinator in the Arrow class..
    "first" converts an arrow from b to c, to an arrow on pairs, that applies its arg to
    the first component and leaves the second component untouched, thus saving its value
    across a computation.                                                              -}
-- we can define a combinator that applies its arg to the second component...
second :: Arrow a => a b c -> a (d,b) (d,c)
second f = arr swap >>> first f >>> arr swap
    where swap (x,y) = (y,x)
{-  arr swap :: a (x,y) (y,x)
    first f :: a (b,d) (c,d)
    arr swap >>> first f :: a (d,b) (c,d)
    arr swap >>> first f >>> arr swap :: a (d,b) (d,c)                -}
-- a combinator which processes both components of a pair...
infixr 3 ***
(***) :: Arrow a => a b c -> a d e -> a (b,d) (c,e)
f *** g = first f >>> second g
-- ..and a combinator which builds a pair from the results of two arrows...
infixr 3 &&&
(&&&) :: Arrow a => a b c -> a b d -> a b (c,d)
f &&& g = arr (\b -> (b,b)) >>> (f***g)
-- with these defns, we can write "add"...
add :: Arrow a => a b Int -> a b Int -> a b Int
add f g = (f&&&g) >>> arr (\(u,v) -> u+v)
-- we can define liftA2 to lift a binary op into an arrow context...
liftA2 :: Arrow a => (b->c->d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>> arr (\(b,c) -> b`op`c)
{-  Although we can construct arrows into a pair type with &&&, and projection arrows such as
    (arr fst) and (arr snd), we can't expect pair types to be a product type in the category
    of arrows, or expect any categorical products to exist. Since arrows represent computations
    which often have side-effects, laws such as " (f&&&g) >>> arr fst = f " do not hold in
    general.

    Why do we use "first" as primitive, rather than, say, (&&&) (which resembles a well-known
    categorical operator)?
    1. Since arrows represent computations with effects, f&&&g is explicit about sequence: it
       represents left-to-right evaluation. However, if &&& were primitive, the evaluation
       order might vary from library to library.
    2. "first" is a simpler operation than (&&&), and its implementation is half the size.
       The implementation of arrow combinators can be complex, and by choosing the simplest
       primitives, we can reduce the work required to build new arrow-based libraries.    -}


-- Arrows and Interpreters.
-- ------------------------
{-  How convenient and expressive are arrow combinators vs monadic ones?
    We'll write fragments of an interpreter based on arrows, and one based on monads.
    If we can write an interpreter in which program fragments in a certain language are
    interpreted as arrows, then we know that we can express any sort of program in the
    interpreted language in terms of arrow combinators.                              -}

-- we begin with a tiny language with only vars and addition...
data Exp = Var String | Add Exp Exp | If Exp Exp Exp
-- the value of such an expression is an Int, but to allow for extensions, we create a type...
data Val = Num Int | Bl Bool
-- we also need a type for environments...
type Env = [(String, Val)]

-- A monadic interpreter maps expressions to computations, represented with a monad M
-- To do this, we use an evaluation function...
evalM :: Exp -> Env -> M Val
type M = IO    -- we'll use the IO monad for now
evalM (Var s) env = return (lookup' s env)
evalM (Add e1 e2) env = liftM2 add (evalM e1 env) (evalM e2 env)
    where add (Num u) (Num v) = Num (u+v)
evalM (If e1 e2 e3) env = evalM e1 env >>= \(Bl b) ->
                          if b then evalM e2 env else evalM e3 env
lookup' s = fromJust . lookup s

-- OTOH, an arrow interpreter maps expressions to computations represented as arrows.
-- what should the input of an expression arrow be? The environment is the natural choice...
evalA :: Exp -> A Env Val
type A env val = env -> val    -- we'll use simply functions as arrows for now
evalA (Var s) = arr (lookup' s)
evalA (Add e1 e2) = liftA2 add (evalA e1) (evalA e2)
    where add (Num u) (Num v) = Num (u+v)

-- Interpreting Conditionals.
-- --------------------------

-- We add a boolean value type and a conditional expression type..
-- the monadic interpreter is easy to extend...see above
-- however, there's a problem with the  arrow interpreter. We could define...
evalA (If e1 e2 e3) = (evalA e1 &&& evalA e2 &&& evalA e3) >>>
                      arr (\(Bl b, (v1,v2)) -> if b then v1 else v2)
{-  but this doesn't capture the intended semantics. This conditional isn't short-circuiting.
    It evaluates both branches and simply chooses between the results.
    This is the crux of the problem: the arrow combinators provide no way to choose between
    2 arrows on the basis of an input. To do that, we need a new combinator.
    Rather than enlarge the existing Arrow class, we define a new class ArrowChoice. Since
    the new combinator will choose between 2 arrows on the basis of the input, it makes sense
    for the input to be of Haskell's sum type, Either a b. We'll define (f|||g) to pass Left
    inputs to f and Right inputs to g. However, just as we chose to define the simpler "first"
    as an arrow primitive in preference to "&&&", so we'll choose a simpler operator than |||
    as the primitive method in ArrowChoice...                                               -}
class Arrow a => ArrowChoice a where
    left :: a b c -> a (Either b d) (Either c d)
-- "left f" invokes f only on Left inputs, and passes Right inputs through unchanged.
-- we can implement "left" for Kleisli Arrows...
instance Monad m => ArrowChoice (Kleisli m) where
    left (K f) = K (\x -> case x of
                            Left b -> f b >>= \c -> return (Left c)
                            Right d -> return (Right d))
-- ...and for regular functions...
instance ArrowChoice (->) where
    left f = \x -> case x of
                     Left b -> Left (f b)
                     Right d -> Right d
-- we can now define further operators...
right :: ArrowChoice a => a b c -> a (Either d b) (Either d c)
right f = arr mirror >>> left f >>> arr mirror
    where mirror (Left x) = Right x
          mirror (Right x) = Left x
infixr 5 <+>
(<+>) :: (ArrowChoice a) => a b c -> a d c1 -> a (Either b d) (Either c c1)
f <+> g = left f >>> right g
infixr 2 |||
(|||) :: (ArrowChoice a) => a b c -> a d c -> a (Either b d) c
f ||| g = (f<+>g) >>> arr untag
    where untag (Left x) = x
          untag (Right x) = x

-- and now we can define conditionals...
evalAC :: Exp -> AC Env Val
type AC env val = env -> val    -- we'll use simply functions as arrows for now
evalAC (Var s) = arr (lookup' s)
evalAC (Add e1 e2) = liftA2 add (evalAC e1) (evalAC e2)
    where add (Num u) (Num v) = Num (u+v)
evalAC (If e1 e2 e3) = (evalAC e1 &&& arr id) >>>
                       arr (\(Bl b, env) -> if b then Left env else Right env) >>>
                       (evalAC e2 ||| evalAC e3)
{-  This is a little more awkward than the monadic version, but can be simplified by the
    introduction of a special combinator for testing predicates. Such a combinator is useful
    enough to be included in a lib, and allows our arrow interpreter to be written as simply
    as the monadic version...                                                              -}
test :: Arrow a => a b Bool -> a b (Either b b)
test f = (f &&& arr id) >>> arr (\(b,x) -> if b then Left x else Right x)
evalAC' (If e1 e2 e3) = test (evalAC e1 >>> arr (\(Bl b) -> b)) >>> 
                        (evalAC e2 ||| evalAC e3)

-- Interpreting Lambda Calculus.
-- -----------------------------
{-  Using the combinators we've created, we could write an arrow interpreter for a first-order
    functional language. But can we interpret HOFs? Let's add lambda-exps and call-by-value
    application to the language.
    To extend the Val type to include functions, we must decide how to represent them.
    Since calling a function may have an effect, we can't use pure funcs Val->Val. In the
    monadic interpreter, we can use functions whose result is a computation (Val->M Val),
    while in the arrow interpreter, we can simply use an arrow (A Val Vsl).                -}
data Exp' = Var' String | Add' Exp' Exp' | Lam' String Exp' | App' Exp' Exp'
data ValM = NumM Int | BlM Bool | FunM (ValM -> MLC ValM)
data ValA = NumA Int | BlA Bool | FunA (A ValA ValA)
-- our monadic eval function is easily extended to handle the new cases...
type MLC = IO
type Env' = [(String, ValM)]
evalM' :: Exp' -> Env' -> MLC ValM
evalM' (Var' s) env = return (lookup' s env)
evalM' (Add' e1 e2) env = liftM2 add (evalM' e1 env) (evalM' e2 env)
    where add (NumM u) (NumM v) = NumM (u+v)
evalM' (Lam' x e) env = return (FunM (\v -> evalM' e ((x,v):env)))
-- case for App' doesn't work...............
