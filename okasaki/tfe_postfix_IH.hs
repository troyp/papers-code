{-# LANGUAGE RankNTypes #-}

import qualified Stack
import Stack (Empty(Empty))

{- We develop postfix DSELs in Haskell. Polymorphism and HOFs play an essential role;
   Lazy evaluation and type classes do not.                                        -}

-- A programming puzzle...
{- consider an RPN-like postfix notation, such that
    begin push 5 push 6 add end  ==>  11
    begin push 5 push 6 push 7 add add end  ==>  18            -}


-- 2. A Solution to the Puzzle.
-- ----------------------------
-- the key is that each command must take the next command as argument
-- so we can determine the types...
begin' ::                 ([Int] -> a) -> a
begin' k = k []
push'  :: [Int] -> Int -> ([Int] -> a) -> a
push' s x k = k (x:s)
add'   :: [Int] ->        ([Int] -> a) -> a
add' (x:y:s) k = k (y+x:s)
end'   :: [Int] -> Int
end' [x] = x
{- Note that the type of every command except begin starts with...
    [Int] -> ...
   and the type of every command except end ends with...
    ... -> ([Int] -> a) -> a
   So, every command except begin takes the current stack as argument, and every
   command except end takes the next command as its final argument. The last action
   of every nonterminal command is to pass the current stack to the next command.  -}

{- The type "([Int] -> a) -> a" and the treatment of the next command suggest
   continuations, but continuations represent the rest of the computation, whereas our
   "continuations" represent only the very next command.
   Thus, our "continuations" are actually *partial continuations".                   -}
{- Consider the evaluation of a simple expression...
        begin push 5 push 6 add end
    =>  push [] 5 push 6 add end
    =>  push [5] 6 add end
    =>  [6,5] add end
    =>  end [11]
    =>  11
   Notice that polymorphism plays a crucial role here...
   In the expression  " begin push 5 push 6 add end "...
   ...the second push is used at type...
    [Int] -> Int                                -- stack, int to be pushed
          -> ([Int] -> ([Int] -> Int) -> Int)   -- type of the "add" funarg
          -> ([Int] -> Int) -> Int              -- partial continuation
   ...while the first push is used at type...
    [Int] -> Int                                         -- stack, int to be pushed
          -> ([Int] -> Int                               -- type of funarg (second push)
                    -> ([Int] -> ([Int] -> Int) -> Int)
                    -> ([Int] -> Int) -> Int)
          -> Int                                         -- int arg to second push
          -> ([Int] -> ([Int] -> Int) -> Int)            -- funarg (add) to second push
          -> ([Int] -> Int) -> Int                       -- partial continuation
   The combination of polymorphism and partial continuations allows us to produce functions
   with greater and greater numbers of args. (Reminiscent of Danvy's functional unparssers) -}


-- 3. Heterogeneous Stacks.
-- ------------------------
{- So far our stacks are just integer lists. There are 2 disadvantages to this...
   1. We would like other types, preferably all Haskell types, available.
   2. We would like the type checker to catch errors related to the size of the stack, eg.
      calling add with only 1 int on the stack, or ending with multiple vals on the stack. -}
{- To satisfy our requirements, we implement stacks as nested pairs (ie. linked lists formed
   from conses). Our stacks grow to the right, which is opposite to the normal implementation
   in a functional language, but consisitent with stack diagrams in postfix languages.
   The types of stacks now encode their exact size and layout. For example, the "only" function
   guaratees that it will only be called on stacks containing exactly one element. However,
   most operations don't need to constrain the entire stack, but only the top few elements.
   Such constraints are handled elegantly as polymorphic types (eg. see "add").             -}
{- Note: in a real application, we would use type classes to overload the numeric operators to
         give them more general types.                                                      -}

{- The main disadvantage of using nested pairs rather than Haskell's native lists to
   represent stacks is that it becomes more difficult to handle situations when varying
   numbers of items may be on the stack.
   We can usually handle this by allowing one or more items on the stack to themselvea be
   lists - it's just as easy to manipulate lists on the stack as integers on the stack (see
   the list operations "nil" and "cons").                                                -}


-- 4. The Postfix Transformation.
-- ------------------------------

-- see Stack.hs
-- copied from Stack.hs...
data EmptyS = EmptyS    -- the empty stack
pushS :: a -> s -> (s, a)
pushS x s = (s,x)
addS :: ((s, Int), Int) -> (s, Int)
addS ((s,x),y) = (s,y+x)
onlyS :: (EmptyS, a) -> a
onlyS (EmptyS, x) = x

-- with these operators, we can express 5+6 as...
ex1 :: Int
ex1 = onlyS (addS (pushS 6 (pushS 5 EmptyS)))
-- we can simulate prefix notation by defining # as left-assoc reverse function application..
infixl 8 #
-- (#) :: a -> (a -> b) -> b  -- sig giving an error - why?
x # f = f x
-- now the above can be rewritten...
ex2 :: Int
ex2 = EmptyS # pushS 5 # pushS 6 # addS # onlyS
-- to abstract away the details of the stack implementation, we define helpers beginS and endS...
beginS = Stack.Empty
endS = Stack.only
-- so we can rewrite the addition as...
ex3 :: Int
ex3 = beginS # pushS 5 # pushS 6 # addS # endS

{- If we wish to use postfix notation directly, without the infix # symbols, we can adapt
   the combinators of section 2 to use our nested-pair representation of stacks.
   However, there's no need to perform the conversion on each operation manually.
   Instead we'll define a few general functions to convert ordinary stack operations into
   postfix stack operations, which we will call "commands".                             -}

-- The type of a typical postfix command is...
type Cmd s s' = forall a. s -> (s' -> a) -> a
{- Here the command takes a stack of type s and returns a stack of type s'.
   The forall construct indicates that each command is polymorphic in the result type of the
   *next* command (to which the result stack is passed).
   This GHC extension simplifies our types considerably.                                   -}

-- to convert an ordinary stack op into postfix form, we use the function "post"...
next :: s -> (s -> a) -> a
next s k = k s
post :: (s -> s') -> Cmd s s'
post f s = next (f s)
-- now we can define postfix stack ops like...
add = post Stack.add
dup = post Stack.dup
-- ...
{- Some ops, like "push", take an arg directly from the insstruction stream rather than from
   the stack. We implement such operations in a similar way...                             -}
type Cmd1 x s s' = forall a. s -> x -> (s' -> a) -> a
post1 :: (x -> s -> s') -> Cmd1 x s s'
post1 f s x = next (f x s)
-- now we can define the other ops...
push = post1 Stack.push
apply = post1 Stack.smap
-- begin and end are simply...
begin :: (Empty -> a) -> a
begin = next Empty
end :: (Empty, a) -> a
end = Stack.only

-- we can now type our example in postfix without the # operator...
ex4 = begin push 5 push 6 add end

-- 5. Extending the Postfix Language.
-- -------------------------------
{- To use postfix notation for more than simple arithmetic expressions, we need more language
   features. We extend our basic language with procedural abstraction, control constructs and
   imperative features.                                                                    -}

-- 5.1. Procedural Abstraction.
-- ----------------------------
{- We'd like to be able to define new commands in postfix notation, like this...
    incr = begindef push 1 add enddef
   which can then be used in programs like this...
    begin push 5 incr incr end    ==>  returns 7
   However, there is a problem: in the defn, what stack should "begindef" pass to "push"?
   There is no appropriate stack. Note that incr is called twice in the program - the stack
   will be different in the two instances.
   What we need is for incr to somehow take the stack on which it will operate.
   To implement this, we change all postfix commands to pass around (stack->stack) functions
   rather than plain stacks.
   A command is then the composition of all operations from the most recent "begin" or
   "begindef" to the current point. When we reach the end command, the function is the
   composition of all the ops in the entire postfix program, and we run it by applying the
   function to Empty.                                                                    -}

-- see Postfix.hs
