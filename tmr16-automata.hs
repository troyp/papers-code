import Data.Set
import Data.Map

type State = Int

data DFA = DFA {
      transitionD :: State -> Char -> Maybe State
    , startD      :: State
    , acceptingD  :: State -> Bool
}
data NFA = NFA {
      transition :: State -> Maybe Char -> Set State
    , start      :: State
    , accepting  :: State -> Bool
}

data RegularExpression a
    = Empty
    | Singleton a
    | Kleene (RegularExpression a)
    | Catenation (RegularExpression a) (RegularExpression a)
    | Alternation (RegularExpression a) (RegularExpression a)
    deriving Show

instance Functor RegularExpression where
    f `fmap` Empty = Empty
    f `fmap` (Singleton a) = Singleton (f a)
    f `fmap` (Kleene r) = Kleene (f `fmap` r)
    f `fmap` (Catenation r s) = Catenation (f `fmap` r) (f `fmap` s)
    f `fmap` (Alternation r s) = Alternation (f `fmap` r) (f `fmap` s)
