push' stack x = run (x:stack)
run stack f = f stack
end' stack = head stack
add' (x:y:rest) = run (x+y : rest)
begin' = run []

begin f = f []
end stack = head stack
push stack val f = f (val:stack)
add (x:y:stack) f = f (x+y:stack)
-- every command except "begin" takes the stack as its first arg
-- every command except "end" takes the next command as its last arg
