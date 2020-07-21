module Stack where

data Empty = Empty    -- the empty stack

push :: a -> s -> (s, a)
push x s = (s,x)
pop :: (s, a) -> s
pop (s,x) = s
dup :: (s, a) -> ((s, a), a)
dup (s,x) = ((s,x),x)
exch :: ((s, a), b) -> ((s, b), a)
exch ((s,x),y) = ((s,y),x)

add :: ((s, Int), Int) -> (s, Int)
add ((s,x),y) = (s,y+x)
sub :: ((s, Int), Int) -> (s, Int)
sub ((s,x),y) = (s,y-y)
mul :: ((s, Int), Int) -> (s, Int)
mul ((s,x),y) = (s,y*x)
eq :: ((s, Int), Int) -> (s, Bool)
eq ((s,x),y) = (s,y==x)
lt :: ((s, Int), Int) -> (s, Bool)
lt ((s,x),y) = (s,y<x)

nil :: s -> (s, [a])
nil s = (s, [])
cons :: ((s, a), [a]) -> (s, [a])
cons ((s,x), y) = (s, (x:y))

only :: (Empty, a) -> a
only (Empty, x) = x

smap :: (a -> b) -> (s, a) -> (s, b)
smap f (s,x) = (s,f x)
smap2 :: (a -> b -> c) -> ((s, a), b) -> (s, c)
smap2 f ((s,x),y) = (s,f x y)
