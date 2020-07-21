    ****************************************************
    *                                                  *
    *   MAP-REDUCE: A 2-page explanation for laymen.   *
    *   Maarten M Fokkinga.                            *
    *                                                  *
    ****************************************************

 =============
 1. Datatypes.
 =============
Consider the datatype of finite binary trees over a set A: it has two 
constructors - Tip (which creates a leaf node out of a value of A) and
Join (which creates a node out of two child nodes).

>    data Tree a = Leaf a
>                | Join (Tree a) (Tree a)
>                  deriving (Show, Eq)
