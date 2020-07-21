--  *****************************************
--  *                                       *
--  *  OPEN DATA TYPES AND OPEN FUNCTIONS.  *
--  *        Andres Loh & Ralf Hinze        *
--  *                                       *
--  *****************************************

{-  The "expression problem" is that of supporting the modular extensibility of both functions
    and data in one language with static type safety. Traditionally, functional languages have
    made it easy to add new functions but not to add new data constructors in a modular faxhion.
    We provide a semantically and syntactically lightweight variant of open datatypes and open
    functions in Haskell.
    The constructors of open datatypes and equations of open functions may appear scattered
    across a program with several modules.
    The intended semantics is as follows: the program should behave as if datatypes and 
    functions are closed, ie. defined all in one place. The order of function equations is
    determined by "best-fit matching": a specific pattern takes precedence over a less-specific
    one.
    We show that our solution is applicable to the expression problem, generic programming and
    exceptions. We discuss two implementations: a direct implementation of the semantics; and a
    scheme based on mutually recursive modules, permitting separate compilation.              -}


-- ================
-- 1. Introduction.
-- ================

-- Consider a simple expression language consisting only of integer constants only...
data Expr = Num Int deriving (Show)
-- We intend to grow the language, adding new features as need arises.

-- Here is an interpreter...
eval :: Expr -> Int
eval (Num n) = n

{-  There are two possible directions to extend the language:
    1. Add new functions, eg. a conversion function from expressions to strings;
    2. Add new forms of expressions by adding new constructors.
    We wish to perform such extensions without modifying existing code.
    Unfortunately, Haskell natively supports only one of these directions. We can easily add a
    new function, such as...                                                                 -}
toString :: Expr -> String
toString (Num n) = show n
{-  ...but we cannot extend our language of expressions: in Haskell, all constructors for a
    datatype must be combined in a single definition.                                     -}

{-  This is the "expression problem". Functional languages make it easy to add new functions,
    but extending data requires modifying existing code. In OO languages, the situation is
    reversed: we can extend data by defining new classes, but adding new methods to those 
    objects requires modification of existing code. Note that use of the visitor pattern can
    simulate the functional situation in an OO language: we gain extensibility for functions,
    but lose it for data at the same time.                                                 -}

{-  Many partial and full solutions to the expression problem have been proposed, but many
    proposals suffer from one or more of the following disadvantages:
    * Focused on OOP and cannot be directly translated to FP.
    * Introduces complex extensions to the type system, eg. mixins or multimethods.
    * Open entities have their own special syntax, or severe limitations, precluding the
      possibility of opening entities after initial definition, so that the programmer must
      decide in advance whether an entity will be open or closed.                         -}

{-  Our lightweight open types and functions will allow us to declare Expr as open, we can add
    new constructors at any time. If we declare a function as open, we can add new equations to
    its definition at any time in the future.                                                 -}
