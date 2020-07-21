-- file: pretty.hs

-- **********************************************
-- *                                            *
-- *  The Design of a Pretty-Printing Library.  *
-- *                John Hughes.                *
-- *                                            *
-- **********************************************

{-  Case study of the design of a combinator library...
    A Pretty-printing library.                -}

data Tree = Node String Tree Tree | Leaf   -- sample datatype
{-  We wish to design a library to display internal data-structures in a readable form.
    Pretty-printing is complicated: the printer must keep track of a lot of contextual
    information. Consider the binary tree...
Node "foo" (Node "baz" Leaf Leaf)
           (Node "foobaz" Leaf Leaf)
    Here, the nodes are laid out in two different ways: some horizontally, some
    vertically. Also, the correct indentation of the final node depends on the length
    of the string in the parent node.                                                -}

-- What kind of objects should pretty-printing combinators manipulate?
{-  We will deal with "pretty-documents" of type Doc. We can think of them as documents
    that know how to lay themselves out prettily. A pretty-printer for a given datatype
    is a function mapping any value of this type to a suitable Doc value. We will
    provide operations for constructing Docs in various ways and for converting Docs to
    text at the top level.                                                            -}
-- we need to convert literal strings to Docs.
-- We would also like to combine Docs horizontally and vertically.
text :: String -> Doc
(<>) :: Doc -> Doc -> Doc    -- horizontal composition
($$) :: Doc -> Doc -> Doc    -- vertical composition
-- the composition operators should automatically handle correct layout.
-- eg. the tree above would be represented by...
--text "Node "foo" " <> (text "Node "baz" Leaf Leaf" $$ text "Node "foobaz" Leaf Leaf")

-- These operations only let us define Docs with a fixed layout.
-- We also need to construct Docs that choose between alternative layouts depending on context
sep :: [Doc] -> Doc    -- combines a list of Docs, horiz. or vert. depending on context

-- we can now write a pretty-printer for the Tree type above...
pp :: Tree -> Doc
pp Leaf = text "Leaf"
pp (Node s l r) = text ("Node " ++ s) <> sep [pp' l, pp' r]
pp' Leaf = pp Leaf
pp' t = text "(" <> pp t <> ")"

-- we would also like an operation which nests a given number of spaces (where appropriate)
nest :: Int -> Doc -> Doc
