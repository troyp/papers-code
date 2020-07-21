

data Var = String
data Env = [env-pair] where env-pair = (Var, Term)
data term = Var Var
          | Lambda Var Term
          | Apply Term Term
          | Closure Env Var Term
