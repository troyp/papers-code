import Prelude hiding ((++))
import Data.Char

-- ********************************
-- *                              *
-- *  MONADIC PARSING IN HASKELL  *
-- *       Hutton & Meijer        *
-- *                              *
-- ********************************

-- tutorial on building recursive descent parsers in Haskell.

-- ======================
-- 2. A Type for Parsers.
-- ======================
newtype Parser a = Parser (String -> [(a, String)])
parse (Parser p) = p
{-  This type can handle ambiguous grammars: an empty result list indicates failure,
    a list of one element represents a unique result and a list of multiple elements
    represents more than one way of parsing the string.
    A given pair consists of a result ::a resulting from parsing a prefix of the string,
    and the remaining unparsed string.                                                -}

-- ======================
-- 3. A Monad of Parsers.
-- ======================
{-  our first parser is "item", which successfully consumes the first character if the string
    is nonempty, or fails otherwise.                                                        -}
item :: Parser Char
item = Parser (\cs -> case cs of
                        ""     -> []
                        (c:cs) -> [(c,cs)])
-- we now define two combinators to make Parser a Monad instance...
instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])
{-  (return a) succeeds without consuming any of the arg string, and returns the single val a.
    (>>=) is a sequencing operator for parsers. The parser (p>>=f) first applies p to the arg
    string cs to obtain a list of results of the form (a,cs'). Then, for each such pair, (f a)
    is a parser which is applied to the string cs'. We end up with a list of lists, which is
    flattened into the final result.
    Note that in p>>=f, f is a function which takes a value of type ::a as its arg, and returns
    a parser as its result. This allows The result of the first parser p to affect the choice of
    the next parser (f a).                                                                -}

-- Monad Laws.
-- -----------
{-  return and (>>=) must obey some simple laws...
    1.  return a >>= f  ==  f a
    2.  p >>= return    ==  p
    3.  p >>= (\a->(f a >>= g))  ==  (p >>= (\a-> f a)) >>= g
    These laws basically say that, modulo the binding operation in >>=, return is a left and
    right unit for >>=, and >>= is associative. The unit laws allow simplification of some
    parsers, and the associativity law allows us to omit parentheses in repeated sequencings. -}

-- ============
-- DO Notation.
-- ============
{-  A typical parser built with >>= has the structure...
p1 >>= \a1 ->
p2 >>= \a2 ->
...
pn >>= \an ->
f a1 a2 ... an
    This parser has a natural operational meaning: apply parser p1 to get result a1; then apply
    p2 to get a2; ... fiinally, combine all results by a semantic action f. This semantic 
    action is usually, but not always, of the form return (g a1 ... an) for some function g.
    Haskell provides a special syntax for monads of the above shape...
do a1 <- p1
   a2 <- p2
   ...
   an <1 pn
   f a1 a2 ... an
    (we can also put the entire thing on a single line if we wish, with braces and semicolons).
    The subexpressions  "ai <- pi"  are called generators, since they genereate values for the
    variables ai. In the special case where we aren't interested in the value generated by a
    generator, we can abbreviate the generator by simply "pi".                              -}
-- example: a parser that consumes 3 chars, discards the 2nd, and returns the others as a pair..
exParser1 :: Parser (Char, Char)
exParser1 = do c <- item
               item
               d <- item
               return (c,d)

-- ===================
-- Choice Combinators.
-- ===================
{-  We now introduce two combinators which extend the monadic nature of parsers, introducing
    the notion of a monad with a zero, and of a monad with a zero and a plus...            -}
class Monad m => MonadZero m where
    zero :: m a
class MonadZero m => MonadPlus' m where
    (++) :: m a -> m a -> m a
-- I've hidden Prelude's ++ so we can generalise it - now to return it by making [] MonadPlus..
instance MonadZero [] where
    zero = []
instance MonadPlus' [] where
    (++) [] ys = ys
    (x:xs) ++ ys = x : xs++ys
-- The type constructor Parser can be made instances of these two classes...
instance MonadZero Parser where
    zero = Parser (\cs -> [])
instance MonadPlus' Parser where
    p ++ q = Parser (\cs -> parse p cs ++ parse q cs)
{-  The parser zero fails for all arg strings, returning no result.
    The (++) operator is a *nondeterministic choice operator* for parsers. ie. the parser
    P++q applies both parsers p and q to the arg string, and appends their lists of results.
    The zero and ++ operations for parsers must obey some eimple laws...
    1.  zero ++ p = p
    2.  p ++ zero = p
    3.  p++(q++r) = (p++q)++r
    These laws hold for all MonadPlus instances. They state that zero is a lift and right
    unit for ++ and that ++ is associative.
    For the special case of parsers, we can also show that zero is the left and right unit
    for >>= (modulo the binding involved in >>=), that >>= distributes through ++ on the
    right, and also on the left if we ignore the order of results...
    4.  zero >>= f  =  zero
    5.  p >>= const zero  =  zero
    6.  (p++q) >>= f  =  (p>>=f) ++ (q>>=f)
    7.  p >>= (\a-> f a ++ g a)  =  (p>>f) ++ (p>>g)
    The zero laws allow some parsers to be simplified, and the distribution laws allow
    some parsers to be made more efficient.                                          -}

{-  parsers built with (++) return many results if the arg can be parsed in different ways.
    Often, we only want the first result. Hence, we define (determinstic) choice operator,
    (+++) which behaves like (++) except that it returns at most one result...           -}
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p++q) cs of
                           [] -> []
                           (x:xs) -> [x])
-- All the laws given for (++) also hold for (+++)
{-  To allow conditional parsing, we define combinator sat that takes a predicate and yields
    a parser that consumes a single char if it satisfies the predicate, or fails otherwise -}
sat :: (Char -> Bool) -> Parser Char
sat pred = do c <- item
              if pred c then return c else zero
-- a parser for specific chars can be defined as...
char c = sat (c==)
-- likewise, we can define parsers for digits, uppercase letters, lowercase letters, etc.

-- ======================
-- Recursion Combinators.
-- ======================
{-  A number of useful parser combinators can be defined recursively. Most can be defined
    for any MonadPlus instance, but we deal only with the case of parsers...            -}
-- parse a specific string...
string :: String -> Parser String
string "" = return ""
string (c:cs) = do char c
                   string cs
                   return (c:cs)
-- parse repeated applications of a parser p: many permits zero or more applications;
many :: Parser a -> Parser [a]
many p = many1 p +++ return []
-- many1 permits 1 or more applications...
many1 :: Parser a -> Parser [a]
many1 p = do a  <- p
             as <- many p
             return (a:as)
-- parse repeated apps of p, seperated by apps of parser sep whose results are discarded...
sepby :: Parser a -> Parser b -> Parser [a]
p`sepby`sep = (p`sepby1`sep) +++ return []
sepby1 :: Parser a -> Parser b -> Parser [a]
p`sepby1`sep = do a <- p
                  as <- many (do {sep; p})
                  return (a:as)
{-  parse repeated apps of p, seperated by apps of a parser op whose result value is an
    operator which is used to combine the results of the p parsers. The operator is assumed
    to be left-associative.                                                               -}
chainl :: Parser a -> Parser (a->a->a) -> a -> Parser a
chainl p op a = (p`chainl1`op) +++ return a
chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
p`chainl1`op = do { a <- p ; rest a }
                      where rest a = (do f <- op
                                         b <- p
                                         rest (f a b)) +++ return a
-- test parsers for reading Ints and minus('-')...
minus :: Parser (Int -> Int -> Int)
minus = Parser (\cs -> case cs of
                      ('-':cs) -> [((\a b -> a-b),cs)]
                      _ -> [])
{-
minus = do char '-'
           return (-)
-}
getDigits :: Parser String
getDigits = do a <- sat isDigit
               as <- many (sat isDigit)
               return (a:as)
int :: Parser Int
int = do s <- getDigits
         return (read s :: Int)
parseIntwithMinus :: Parser Int
parseIntwithMinus = chainl int minus 0
-- In a similar way, we can define combinators chainr and chainr1 assuming right-associativity
chainr :: Parser a -> Parser (a->a->a) -> a -> Parser a
chainr p op init = (p`chainr1`op) +++ return init
chainr1 :: Parser a -> Parser (a->a->a) -> Parser a
-- *********WRONG!!!**********
-- TODO: FIX!!!!!!!!!!!
{-
p`chainr1`op = do a <- p
                  f <- op
                  return (rest a (f a)) +++ return a
                      where rest a g x = do
                              x <- p`chainr1`op
                              return (g x) ++ return a
-}

p`chainr1`op = do a <- p
                  f <- op
                  b <- p`chainr1`op
                  return (f a b) +++ return a

{-
p`chainr1`op = do a <- p
                  next a id +++ return a
                      where next a fn = do 
                              f <- op
                              b <- p
                              let iflast = f a b
                              let notlast = \x-> fn (f a x)
                              r <- next b notlast
                              return (notlast r) ++ return iflast
-}
{-
rightAssoc l op r = \g -> (l`op`) . (g r)
p`chainr1`op = do a <- p
                  next a +++ return a
                      where next a = do 
                              f <- op
                              b <- p
                              let iflast = f a b
                              r <- next b
                              next b ++ return iflast
-}
{-
p`chainr1`op = do a <- p
                  r <- rest a
                  return r +++ a
                      where rest a = do
                              f <- op
                              b <- p`chainr1`op
                              return (f a b)
-}
{-
p`chainr1`op = Parser helper
    where helper p op cs =
              let [(a, cs')] = parse p cs
                  [(f, cs'')] = parse op cs'
                  [(b, cs''')] = helper p op cs''
              in [(f a b), cs''']
-}
parseIntwithRightAssocMinus :: Parser Int
parseIntwithRightAssocMinus = chainr int minus 0


-- =======================
-- 7. Lexical Combinators.
-- =======================
{-  Parsing is traditionally preceded by a lexing phase that transforms the arg string into a
    sequence of tokens. We can avoid this by defining suitable combinators. We can define
    lexical combinators to handle issues like the use of space to delimit tokens, handling of
    comments and keywords, etc.                                                             -}
-- parse a string of spaces/tabs/newlines...
space :: Parser String
space = many (sat isSpace)
-- parse a token with parser p, discarding trailing space...
token :: Parser a -> Parser a
token p = do a <- p
             space
             return a
-- parse a symbolic token...
symb :: String -> Parser String
symb cs = token (string cs)
-- apply a parser p, discarding leading space...
apply :: Parser a -> String -> [(a, String)]
apply p = parse (do space
                    p)



-- ===========
-- 8. Example.
-- ===========
{-  We illustrate the parser combinators with a simple example: a standard grammar for
    arithmetic, built from single digits and the operators +,-,*,/, plus parentheses -
expr   =: expr addop term | term
term   =: term mulop factor | factor
facto  =: digit | (expr)
digit  =: 0|1|2|3|4|5|6|7|8|9
addop  =: +|-
mulop  =: *|/
    Using chainl1 to implement the left-recursive production rules for expr and term, we
can directly translate this grammar into a Haskell prgram that parses expressions and
    evaluates them to their integer value.                                             -}
expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term`chainl1`addop
term = factor`chainl1`mulop
factor = digit +++ do symb "("
                      n <- expr
                      symb ")"
                      return n
digit = do x <- token (sat isDigit)
           return (ord x - ord '0')
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return div}
