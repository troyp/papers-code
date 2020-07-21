-- * BACKUS: Can Programming be Liberated from the von Neumann Style? *
-- 
-- construction.
construction fn_ls = (flip map fn_ls) . flip ($)
construction_ex = construction [(^2), (/2), (+1).(*2)]
