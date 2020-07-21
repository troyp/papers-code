class Bifunctor s where
  bimap :: (a->b) -> (c->d) -> s a c -> s b d

data Fix s a = In { out:: s a (Fix s a)}

instance Bifunctor (,) where
  bimap f g (x,y) = (f x, g y)
