

-- ***********************************************************************
-- *                                                                     *
-- * Views: A Way for Pattern Matching to cohabit with Data Abstraction. *
-- *                                                                     *
-- ***********************************************************************

-- Viewing an Integer as Zero or a Successor.
-- ------------------------------------------
-- view int ::= Zero | Succ Int
--     in n
--       | n == 0  =  Zero
--       | n >  0  =  Succ (n-1)
--     out Zero      =  0
--     out (Succ n)  =  n+1
data Viewtype = Zero | Succ Int
viewin n
  | n == 0  =  Zero
  | n >  0  =  Succ (n-1)
viewout Zero     = 0
viewout (Succ n) = n+1
