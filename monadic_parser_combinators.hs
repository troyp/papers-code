{-# LANGUAGE TypeOperators #-}
-- ********************************
-- *                              *
-- *  MONADIC PARSER COMBINATORS  *
-- *       Hutton & Meijer.       *
-- *                              *
-- ********************************
import Prelude hiding ((++))    -- redefined later more generally on MonadPlus
import qualified Prelude as Pre
import Control.Monad


-- ======================
-- 2. Combinator Parsers.
-- ======================
-- 2.1. The Type of Parsers.
-- -------------------------
{-  We can start by thinking of a parser as a function taking a string of chars and yielding
    some kind of tree expressing the grammatical structure of the string...
    type Parser = String -> Tree
    However, a parser may not consume all its input, so we should return any unused chars...
    type Parser = String -> (Tree, String)
    Also, a parser may  fail on an input string; it may also be possible to parser an input in
    more than one way if the underlying grammar is ambiguous. We can allow for these
    possibilities by returning a list of results rather than an individual result...
    type Parser = String -> [(Tree, String)]
    We may want to use different kinds of trees for different parsers, so we will abstract on
    the type of tree and make the result type a parameter of the Parser type...             -}
newtype Parser a = Parser { runP :: String -> [(a, String)] }
{-  We could also abstract upon the type String of tokens, but won't do so here.
    See Hutton, "Higher Order Functions for Parsing".                          -}

-- 2.2. Primitive Parsers.
-- -----------------------
-- we define 3 primitive parsers which are the building blocks of combinator parsing...

-- result v: always succeeds and returns its arg without consuming any of the string...
result :: a -> Parser a
result v = Parser $ \inp -> [(v, inp)]
-- zero: always fails...
zero :: Parser a
zero = Parser $ \inp -> []
-- item:: Parser Char consumes the first char, which is its result; it fail on an empty string..
item :: Parser Char
item = Parser $ \inp -> case inp of
                          [] -> []
                          (x:xs) -> [(x, xs)]

-- 2.3. Parser Combinators.
-- ------------------------
{-  The primitive parsers aren't very useful by themselves, but we can glue them together to
    make useful parsers. We take our lead from BNF notation for grammars which builds larger
    grammars from smaller using two operators: a sequencing operator (juxtaposition) and a
    choice operator (|). We define similar operators for our parsers, so the structure of our
    parsers closely follows that of the underlying grammar.
    In earlier, non-monadic, accounts of combinator parsing, sequencing was captured by a
    combinator "seq" that applied one parser after another, with the results combined in pairs.
    At first sight, this may seem like a natural parsing operation, but it results in nested
    pairs, which are hard to manipulate.
    This can be avoided by using a monadic sequencing combinator (bind), which integrates the
    sequencing of parsers with the processing of their return values.                       -}
bind :: Parser a -> (a -> Parser b) -> Parser b
(Parser p) `bind` f = Parser $ \inp -> concat [(f v) `runP` inp' | (v, inp') <- p inp]
{-  So, when (p`bind`f) is applied to input inp, the parser p first operates on inp, yielding a
    list of (value, string) pairs. Now f is applied to the value of each pair, yielding a parser
    which is applied to the corresponding string of the pair, resulting in a list of 
    value, string for each element of the first list. This list of lists is then flattened into
    a single list with concat.                                                                -}
{-  The bind combinator avoids the problem of nested tuples because the results of the first
    parser are made directly available for processing by the second, rather than being paired up
    with other results for processing later on.
    A typical parser built with bind has the structure...
    p1 `bind` \x1 ->
    p2 `bind` \x2 ->
    ...
    result (f x1 x2 ...)                                                                     -}
-- for instance, seq can be defined as...
seq :: Parser a -> Parser b -> Parser (a,b)
p`seq`q = p `bind` \x ->
          q `bind` \y ->
          result (x,y)
-- note that bind can *not* be defined in terms of seq.

-- We can now define some simple but useful parsers...

{- "item" consumes a single char unconditionally. In practice, we're normally only interested in
   consuming certain specific chars.                                                          -}
-- sat pred: consumes a single char if it satisfies pred, and fails otherwise...
sat :: (Char -> Bool) -> Parser Char
sat pred = item `bind` \x ->
           if pred x then result x else zero
{-  Note: if item fails (ie. if the input string is empty), then so does
    sat pred, since we can easily see that
    zero `bind` f = zero   for all function f (of appropriate type)
    Indeed, this equation holds generally for any monad with a zero.                -}

-- Using sat, we can define parsers for certain classes of character...

-- char x:  Parser that consumes a char only if it is x
char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x ->  '0' <= x  &&  x <= '9')

lower :: Parser Char
lower = sat (\x ->  'a' <= x  &&  x <= 'z')

upper :: Parser Char
upper = sat (\x ->  'A' <= x  &&  x <= 'Z')

{-  As another example of using bind, consider this parser that accepts two lowercase letters in
    wequence, returning a string of length 2...                                              -}
parseTwoLowercase :: Parser String
parseTwoLowercase = lower `bind` \x ->
                    lower `bind` \y ->
                    result [x,y]
ex1 = parseTwoLowercase `runP` "abcd"    -- [("ab", "cd")]
ex2 = parseTwoLowercase `runP` "aBcd"    -- []

{-  This two-letter parser can be generalised to a parser for arbitrary strings of lowercase
    letters. Since the length of the string to be parsed cannot be known in advance, such a
    parser will naturally be defined recursively.
    This requires a *choice combinator* which can decide between parsing another letter and
    recursing, or parsing nothing more and terminating.
    A suitable choice combinator is...                                              -}
-- plus p q: applies both parsers to the input and combines their results.
plus :: Parser a -> Parser a -> Parser a
p`plus`q = Parser $ \inp -> (p`runP`inp ++ q`runP`inp)
{-  Note that there is no requirement that p and q accept disjoint sets of strings: if both
    parsers succeed on the input string then more than one result value will be returned,
    reflecting the different ways the string can be parsed.                              -}

-- examples of using plus...

letter :: Parser Char
letter = lower`plus`upper

alphanum :: Parser Char
alphanum = letter`plus`digit

-- most interestingly, a parser for words (strings of letters) is defined by...
word :: Parser String
word = neWord `plus` result "" where
    neWord = letter `bind` \x ->
             word `bind` \xs ->
             result (x:xs)
{-  nonempty words (note: the above defn for words could be separated into mutually recursive
    defintions for both nonempty and posiibly-empty words)...                              -}
word1 = letter `bind` \x ->
        (word1 `plus` result "") `bind` \xs ->
        result (x:xs)

ex3 = word `runP` "Yes!"   -- [("Yes","!"),("Ye","s!"),("Y","es!"),("","Yes!")]
{-  The first result, ("Yes","!"), is the "expected" result: all consecutive letters have been
    consumed, and the uncomsumed input is "!". In the subsequent results, a decreasing number of
    letters are consumed.
    This behaviour arises because "plus" is *nondeterministic*: both alternatives are explored,
    even when the first alternative is successful. Hence there is always the option to finish
    parsing, even when there are still unconsumed letters left.                             -}


-- ======================
-- 3. Parsers and Monads.
-- ======================
-- The Parser Monad.
-- -----------------
instance Monad Parser where
    return = result
    (>>=)  = bind
instance MonadPlus Parser where
    mzero = zero
    mplus = plus
-- infix operator for nondeterministic choice operator...
(++) :: (MonadPlus m) => m a -> m a -> m a
(++) = mplus
infixr 5 ++
