posints = 1 : map (1+) posints

rec f = ds
    where ds = f ds
posints' = rec $ (1:) . map (1+)

ones = 1 : ones

-- merge two lists of ascending elements into a single list with no duplication
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
          | x==y   = x : merge xs ys
          | x<y    = x : merge xs (y:ys)
          | x>y    = y : merge (x:xs) ys

merge3 :: Ord a => [a] -> [a] -> [a] -> [a]
merge3 xs ys zs = merge xs (merge ys zs)    -- efficiency ?
