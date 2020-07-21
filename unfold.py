

def foldr(op, init, lst):
    if not lst: return init
    last = lst.pop()
    return op(lst,
              init,
              last)
def unfold(pred, term, next, x):
    result = []
    while not pred(x):
        result.append(term(x))
        x = next(x)

class Tree(object):
    def __init__(self, label, subtrees=[]):
        super(Tree, self).__init__(label, subtrees)
        self.label = label
        self.subtrees = subtrees
    def __str__(self):
        return "Tree " + str(self.label) + " " + str(self.subtrees)
    def __repr__(self):
        return "Tree " + str(self.label) + " " + str(self.subtrees)
