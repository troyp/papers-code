-- ************************************************************************
-- *                                                                      *
-- *  Parsec: Direct Style Monadic Parser Combinators for the Real World  *
-- *                         Leijen & Meijer                              *
-- *                                                                      *
-- ************************************************************************

{-  Naive implementations of parser combinators suffer from space leaks and are often unable
    to report precise error messages.
    The Parsec parser combinator library is time and space-efficient and in the case of a parse
    error, reports the position of the error and all productions which would have been legal at
    that point.                                                                              -}

-- 1. Introduction.
-- ----------------
{-  Parser combinators are a favourite FP topic. While other parsers offer only a fixed set of
    combinators, combinator parsers allow combinators to be manipulated as first class values
    and combined to define new combinators suited to the application domain. Another advantage
    is that the programmer uses only one language, rather than having to integrate different
    tools and languages.
    Unfortuately, many such libraries are not suited to real world use due to inefficiency,
    space leaks and/or poor error messages.                                               -}
{-  Parsec avoids these problems. Topics discussed...
    * A novel implementation technique for space- and time-efficient parser combinators.
      Laziness is essential in the short, conxise implementation.
      We identify a space leak afflicting many existing parser combinators described in the
        literature.
    * Primitive combinators can be naturally extended with error messages.
      The user can label grammar productions with suitable names.
      Ther error messages report not only the position of the error, but also all productions
        that would have been legal at that point in the input (ie. the first set of that
        production).                                                                       -}


-- 2. Grammars and Parsers.
-- ------------------------
