{-# LANGUAGE RankNTypes #-}

module Postfix where

import qualified Stack
import Stack (Empty(Empty))

type Cmd s s' = forall s0 a. (s0 -> s) -> ((s0 -> s') -> a) -> a
{- The post function now composes the new function with the old one, rather than applying the
   function to a stack...                                                                  -}
next :: s -> (s -> a) -> a
next s k = k s
post :: (s -> s') -> Cmd s s'
post f ss = next (f . ss)
-- the definitions of the basic postfix commands remains the same...
add :: Cmd ((s, Int), Int) (s, Int)
add = post Stack.add
dup :: Cmd (s, a) ((s, a), a)
dup = post Stack.dup

-- Cmd1 and post1 are defined similarly...
type Cmd1 x s s' = forall s0 a. (s0 -> s) -> x -> ((s0 -> s') -> a) -> a
post1 :: (x -> s -> s') -> Cmd1 x s s'
post1 f ss x = next (f x . ss)
-- and we can now define the postfix commands taking an arg from the instruction stream...
push = post1 Stack.push
apply = post1 Stack.smap
-- the begin and end commands are...
begin :: ((Empty -> Empty) -> a) -> a
begin = next id
end :: (Empty -> (Empty, a)) -> a
end ss = Stack.only (ss Empty)
{- The arg is the final state->state function, composed of all the previous ones, taking
   s0=Empty to the final state (Empty, a), which holds the single element of type a which will
   will be the result of the program.                                                       -}

-- We can now write "begindef" and "enddef"; they turn out to be surprisingly simple...
begindef :: ((s -> s) -> a) -> a
begindef = next id
enddef :: (s -> s') -> Cmd s s'
enddef = post
{- We see that "begindef" is similar to "begin", but with a more general type;
   "enddef" turns out to be identical to "post"!                            -}

-- we can now define "incr"...
incr :: Cmd (s, Int) (s, Int)
incr = begindef push 1 add enddef
