--  **************************************************
--  *                                                *
--  * The Haskell Programmers Guide to the IO Monad. *
--  *                 Stefan Klinger.                *
--  *                                                *
--  **************************************************

import Char

-- interactive "hello world" program
hello_main :: IO ()
hello_main = do putStr "What's your name?\n"
                x <- getLine
                putStr . concat $ ["Hello, ", x, ".\n"]

{-  We consider the "obvious" category in Haskell:
    * objects are Haskell types (primitive and constructed)
    * morphisms are (unary) Haskell functions
    * type information is given by the type signature of a function
    * composition of morphisms is Haskell function composition (.)
    * the identity is typed    id :: forall a. a -> a    -}

-- Functors.
-- ---------
{-  Let A*,B* be categories. Then 2 mappings F(O):O(A)->O(B) and F(M):M(A)->M(B)
    together form a functor F:A->B if they satisfy...
    1. preservation of type info
       forall f : A-(A*)->B,   F(M)f : F(O)A-(B*)->F(O)B
    2. preservation of identities
       forall A in O(A), F(A) id(A) == id(F(O)A)
    3. distributivity under composition of morphisms
       forall f : A-(A*)->B ; g : B-(A*)->C, 
       F(M)(g (.A*) f) == F(M)g (.B*) F(M)f
    Haskell has a Functor typeclass, requiring a function fmap with the correct
    type signature (so 1. is fulfilled). However, to ensure that an instance is
    a true functor, the latter 2 conditions above must be checked manually.
    They amount to...
    *  fmap is == id
    *  fmap (g . f) == fmap g . fmap f   must be true for all functions f,g.
    Maybe and lists are both functors.                -}

-- Natural Transformations.
-- ------------------------
