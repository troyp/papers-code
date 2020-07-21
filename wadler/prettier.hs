-- *************************
-- *                       *
-- *  A PRETTIER PRINTER.  *
-- *    Philip Wadler.     *
-- *                       *
-- *************************

{-  We develop a pretty-printing library improving the one designed by Hughes.
    Hughes's library has both a horizontal and a vertical concatenator, with
    the horizontal composition possessing a right unit but no left unit, and
    the norizontal composition possessing neither unit.
    We base this new library on a single way to concatenate documents, which
    is associative with both left and right units.
    The algorithm used is similar to a functional version of Oppen's pretty
    printer.                                                               -}

import Data.List (replicate)
infixr 5 :<|>
infixr 6 :<>
infixr 6 <>

-- ===========================
-- 1. A Simple Pretty Printer.
-- ===========================

{-    Consider first the simple case where a document has only one possible
      structure, ie. we do not attempt to compress structure onto one line -}
(<>) :: Doc -> Doc -> Doc    -- document concatenation (associative)
nil  :: Doc                  -- left and right unit of <>
text :: String -> Doc        -- converts a string to corresponding Doc
line :: Doc                  -- represents a line break
nest :: Int -> Doc -> Doc    -- adds indentation
layout :: Doc -> String      -- converts a document to a string for output

{-  One simple implementation of Doc would be as strings, with
    (<>) = (++) ; nil = "" ; text = id ; line = "\n" ; layout = id ;
    nest n = foldr (\c s-> if c=='\n' then (c:(replicate n ' ')++s) else (c:s)) ""
    We will consider a more algebraic implementation soon.
    Note: indentation is added only at an explicit newline, not at the beginning
          of the string. This is in contrast with Hughes's implementation, but
          we will see that it is this choice which enables us to use only a single
          concatenation operator rather than two.                                -}

-- example: a sample Tree datatype with functions to convert Tree values to Doc values
data Tree = Node String [Tree]
exTree = Node "aaa" [(Node "bbbb" [(Node "ccc" []), (Node "dd" [])]),
                     (Node "eee" []),
                     (Node "ffff" [(Node "gg" []), (Node "hhh" []), (Node "i" [])])]

showTree :: Tree -> Doc
showTree (Node s ts) = text s <> nest (length s) (showBracket ts)
showBracket :: [Tree] -> Doc
showBracket [] = nil
showBracket ts = text "[" <> nest 1 (showTrees ts) <> text "]"
showTrees :: [Tree] -> Doc
showTrees [t] = showTree t
showTrees (t:ts) = showTree t <> text "," <> line <> showTrees ts
-- here is a variant conversion function
showTree' :: Tree -> Doc
showTree' (Node s ts) = text s <> showBracket' ts
showBracket' :: [Tree] -> Doc
showBracket' [] = nil
showBracket' ts = text "[" <> nest 2 (line <> showTrees' ts) <> (line <> text "]")
showTrees' :: [Tree] -> Doc
showTrees' [t] = showTree' t
showTrees' (t:ts) = showTree' t <> text "," <> line <> showTrees' ts

-- Normal Form of a Document.
{-  Every document can be reduced to a *normal form* of text alternating with line
    breaks nested to a given indentation. eg..                                -}
exDoc =
    text "bbbbb" <> text "[" <>
    nest 2 (
            line <> text "ccc" <> text "," <>
            line <> text "dd"
           ) <>
    line <> text "]"
--  has normal form...
exDocNormal =
    text "bbbbb[" <>
    nest 2 line <> text "ccc," <>
    nest 2 line <> text "dd" <>
    nest 0 line <> text "]"
{-  Hence, the document prints as...
bbbbb[
  ccc,
  dd
]
                                                                                -}
-- Laws for reducing a Document to Normal Form.
{-  The following rules reduce a document to normal form, together with the fact
    that <> is associative with nil. Note that all but the last law come in pairs:
    a law on a binary operator paired with a law for its unit.

    text (s ++ t)     = text s <> text t
    text ""           = nil

    nest (i + j) x    = nest i (nest j x)
    nest 0 x          = x

    nest i (x <> y)   = nest i x <> nest i y
    nest i nil        = nil

    nest i (text s)   = text s                                                  -}

-- Laws relating a Document to its Layout.
{-  layout (x <> y)       = layout x ++ layout y    -- layout is a homomorphism from Doc
    layout nil            = ""                      -- concatenaton to String concatenation
    layout (text s)       = s    -- layout is the (left-)inverse of text (see first 2 laws)
    layout (nest i lines) = '\n' : copy i ' '    -- layout of a nested line is \n followed by
                                                 -- one space for each level of indentation    -}
-- A Simple Implementation.
{-  A simple implemenation can be derived directly from the algebra of documents above:
    we represent a document as a concatenation of items, each of which is either a text or a
    line break with indentation.
data Doc = Nil                    -- Nil ~ nil
         | String `Text` Doc      -- Text s x ~ text s <> x
         | Int `Line` Doc         -- Line i x ~ nest i line <> x
         deriving (Show)
-}
-- representations of our functions are easily derived from the above equations...
nil = Nil
text s = Text s Nil
line = Line 0 Nil
{-
(s `Text` x) <> y  =  s `Text` (x <> y)
(i `Line` x) <> y  =  i `Line` (x <> y)
Nil <> y  =  y

nest i (s `Text` x)  =  s `Text` nest i x
nest i (j `Line` x)  =  (i+j) `Line` nest i x
nest i Nil  =  Nil
-}
layout (s `Text` x)  =  s ++ layout x
layout (i `Line` x)  =  '\n' : replicate i ' ' ++ layout x
layout Nil  =  ""


-- =============================================
-- 2. A Pretty Printer with Alternative Layouts.
-- =============================================

{-  We will now consider documents with multiple possible layouts.
    In the last section, we could consider a document to be equivalent to a string -
    now, we will view a document as equivalent to a set of strings, each corresponding
    to a different layout of the same document. This extension can be acieved by adding
    a single function...                                                            -}
group :: Doc -> Doc
{-  Given a document, which represents a set of layouts, group returns the set with one
    new element added: the layout which compresses everything onto a single line. This
    new layout is formed by replacing each newline (and associated indentation) with a
    single space.
    The function "layout" is replaced with one that chooses the "prettiest" among a set
    of layouts: it takes an additional parameter: the preferred maximum line width.    -}
pretty :: Int -> Doc -> String

-- example: a revision of showTree...
showTree'' (Node s ts) = group (text s <> nest (length s) (showBracket ts))

-- To formalize the semantics of the new operations, we add two auxillary operations...
(<|>) :: Doc -> Doc -> Doc
flatten :: Doc -> Doc
{-  The (<|>) operator forms the union of two sets of layouts.
    "flatten" replaces each line break and associated indentation, with a single space.
    A document always represents a non-empty set of layouts, each of which flattens to
      the same layout.
    As an invariant, we require that in (x <|> y), all layouts in x and y flatten to the
      same layout.
    Laws extend each operator on simple documents pointwise through union...
      (x <|> y) <> z    ==  (x <> z) <|> (y <> z)
      x <> (y <|> z)    ==  (x <> y) <|> (x <> z)
      nest i (x <|> y)  ==  nest i x <|> nest i y
    Since flattening gives the same result for each element of a set, its distributive
    law is simpler...
      flatten (x <|> y)  =  flattern x
    Finaly, laws on "flatten"'s interaction with other document constructors...
      flatten (x <> y)   ==  flatten x <> flatten y
      flatten nil        ==  nil
      flatten (text s)   ==  text s
      flatten line       ==  text " "
      flatten (nest i x) ==  flatten x                                                -}
-- Now we can define "group" in terms of flatten and <|>...
-- group x = flatten x <|> x
-- These laws are adequate to convert any document to normal form
{-  Now we need to specify how to chhose the best layout from a set.
    Like Hughes, we shall specify an ordering relation between lines, and then extend it
v    lexically to form an ordering relation between documents.
    The ordering relation is dependant on the preferred maximum width W.
    If both lines are shorter than W, the longer is better.
    If one fits within W and the other doesn't, the one that fits is better.
    If both are longer than W, the shorter is better.
    (Note that (unlike Hughes), we accept a line longer than W if necessary.)

    One possible implementation is to consider sets of layouts, where sets are lists, and
    layouts are either strings or the algebraic representation of the previous section.
    This is hopelessly inefficient, suffering a combinatorial explosion as the number of
    choices increases.
    Forunately, the algebraic formulation above is easily extended to give a tractable
    representation. We need only add a construct representing the union of documents.    -}
data Doc = Nil                  -- Nil = nil
         | String `Text` Doc    -- s `Text` x  = text s <> x
         | Int `Line` Doc       -- i `Line` x  = nest i line <> x
         | Doc `Union` Doc      -- x `Union` y = x <|> y
         deriving (Show)
(<|>) = Union
{-  We now have 2 invariants on (x `Union` y):
      * as before, x and y must flatten to the same document
      * additionally, we require that no first line of any document in x is shorter than
        the first line of any document in y                                            -}
-- <> and nest must be extended to specify their interaction with Union...
nest i (s `Text` x)  =  s `Text` nest i x
nest i (j `Line` x)  =  (i+j) `Line` nest i x
nest i Nil  =  Nil
nest k (x `Union` y)  =  nest k x `Union` nest k y
(s `Text` x) <> y  =  s `Text` (x <> y)
(i `Line` x) <> y  =  i `Line` (x <> y)
Nil <> y  =  y
(x `Union` y) <> z  =  (x <> z) `Union` (y <> z)

-- note that these preserve the length x >= length y in (x <|> y) invariant
{-  To improve performance, we will exploit the distributive law, and use the representation
    (s `Text` (x `Union` y)) over the equivalent ((s `Text` x) `Union` (s `Text` y)).    -}
-- eg. Consider the document...
exGroupDoc = group (group (group (group (text "hello" <> line <> text "a")
                                  <> line <> text "b")
                           <> line <> text "c")
                    <> line <> text "d")

{-  This has the following possible layouts...
hello a b c            hello a b            hello a            hello
                       c                    b                  a
                                            c                  b
                                                               c
    To lay this out with a width of 5, we must pick the last of these, and would like to
    eliminate the others in a single swoop. To acieve this, we choose a representation that
    brings to the front any common string, eg...
"hello" `Text` ((" " `Text` x) `Union` (0 `Line` y))
    for suitable documents x and y. Here, "hello" has been factored out of all layouts in x and y,
    and "hello" followed by " " has been factored out of all layouts in x. Since "hello" followed
    by " " occupies 6 characters and "hello" occupies 5, we may immediately choose the right
    operand of Union without any further examination of x, as desired.                        -}
-- using the laws above, we can easily derive definitions of group and flatten
group Nil = Nil
group (i `Line` x) = (" " `Text` flatten x) `Union` (i `Line` x)
group (s `Text` x) = s `Text` group x
group (x `Union` y) = group x `Union` y

flatten Nil = Nil
flatten (i `Line` x) = " " `Text` flatten x
flatten (s `Text` x) = s `Text` flatten x
flatten (x `Union` y) = flatten x

{-  Now we must choose the best among a set of layouts. We use a funviton "best" which takes a
    document which may contain unions, and returns a document containing no unions. It takes two
    additional parameters: available width w, and the number of characters k already placed on
    the current line (including indentation).                                                -}
best :: Int -> Int -> Doc -> Doc    -- post-condition: return Doc contains no unions.
best w k Nil = Nil
best w k (i `Line` x) = i `Line` best w i x
best w k (s `Text` x) = s `Text` best w (k + length s) x
best w k (x `Union` y) = better w k (best w k x) (best w k y)
better w k x y = if fits (w-k) x then x else y
fits :: Int -> Doc -> Bool    -- pre-condition: Doc argument contains no unions
fits w x | w < 0 = False
fits w Nil = True
fits w (s `Text` x) = fits (w - length s) x
fits w (i `Line` x) = True
{-  When applying "best" to the union of x and y, the better of best x and best y is selected.
    It is essential for efficiency that the inner computation of best is performed lazily.
    By the invariant for unions, no first line in x may be shorter than any first line in y -
    hence, by the criterion given above, x is chosen if it fits, y otherwise.                -}

pretty w x = layout $ best w 0 x


-- ========================
-- 3. Improving Efficiency.
-- ========================

{-  Our implementation above is reasonably efficient, but can be improved.
    We would like pretty-printing to be achievable in time O(s) and space O(w max d)
    where s = length of the doc = (# of <>, nil, text, nest and group ops) + (length of doc)
    and w = available width, d = depth of the doc (depth of calls to nest or group)

    There are 2 sources of inefficiency...
    1. Concatenation of documents might pile up to the left...
       (...(text s0 <> text s1) <>...) <> text sn
       Ideally, this would take O(n), but if all strings were length 1, for instance, it would
       be O(n^2).
    2. Even when concatenation associates to the right, nesting of documents adds a layer of
       processing to increment the indentation of the inner document.
       nest i0 (text s0 <> nest i1 (text s1 <> ... <> nest in (text sn)...))
       Again, this is O(n) at best, O(n^2) at worst (all strings length 1)                -}
{-  A possible fix for the first problem is to add an explicit representation for concatenation,
    and generalize each operation to act on a list of concatenated documents.
    A possible fix for the second problem is to add an explicit representation for nesting, and
    maintain a current indentation which is incremented as nesting operations are processed.
    *  Combining these two fixes suggests generalizing each operation to work on a list of
    indentation-document pairs.                                                                -}

{-  To implement this, we introduce a new representation for documents, with one constructor for
    each operator that builds a document.                                                    -}
data DOC = NIL
         | DOC :<> DOC
         | NEST Int DOC
         | TEXT String
         | LINE
         | DOC :<|> DOC
         deriving (Show)
{- The operators for building a document are trivially defined...
nil  = NIL
(<>) = (:<>)
nest = NEST
text = TEXT
line = LINE
    we will not define these so as not to conflict with the previous implementation.            -}

{-  Again, as invariants for :<|>, we reauire that in (x :<|> y)...
    1. all layouts in x and y flatten to the same layout
    2. no first line in x is shorter than any first line in y                                -}

-- definitions of group and flatten are straightforward...
group' :: DOC -> DOC
group' x = flatten' x :<|> x
flatten' :: DOC -> DOC
flatten' NIL        = NIL
flatten' (x :<> y)  = flatten' x :<> flatten' y
flatten' (NEST i x) = flatten' x
flatten' (TEXT s)   = TEXT s
flatten' LINE       = TEXT " "
flatten' (x :<|> y) = flatten' x

{-  The representation function maps a list of indentation-document pairs into the
    corresponding document.                                                        -}
rep :: [(Int, DOC)] -> DOC
rep z = foldr (:<>) NIL [ NEST i x | (i,x) <- z ]

{-  The operation to find the best layout is generalized to act on a list of
    indentation-document pairs. The generalized operation is defined as the
    composition of the old operation and the representation function...
hypthesis:    be w k z = best w k (rep z)
    The new definition of "best" is easily derived from the old using the hypothesis
    above and the laws and definitions already covered.                                -}
best' :: Int -> Int -> DOC -> Doc
best' w k x = be w k [(0, x)]
be :: Int -> Int -> [(Int, DOC)] -> Doc
be _ _ []                  = Nil
be w k ((_, NIL) : z)      = be w k z
be w k ((i, x :<> y) : z)  = be w k ((i,x):(i,y):z)
be w k ((i, NEST j x) : z) = be w k ((i+j,x):z)
be w k ((i, TEXT s) : z)   = s `Text` be w (k + length s) z
be w k ((i, LINE) : z)     = i `Line` be w i z
be w k ((i, x :<|> y) : z) = better w k (be w k ((i,x):z)) (be w k ((i,y):z))

{-  While the argument to best' is represented using DOC, the result is represented using
    the older represented Doc. Thus the function pretty' can be defined as before...    -}
pretty' w x = layout $ best' w 0 x


-- ============
-- 4. Examples.
-- ============

-- we can define a number of convenience functions...
(<+>) :: DOC -> DOC -> DOC
x <+> y = x :<> TEXT " " :<> y
(</>) :: DOC -> DOC -> DOC      -- vertical concatenation operator: see below & Hughes' pretty
x </> y = x :<> LINE :<> y      -- printer. Has neither left nor right unit.

folddoc :: (DOC -> DOC -> DOC) -> [DOC] -> DOC
folddoc f [] = NIL
folddoc f [x] = x
folddoc f (x:xs) = f x (folddoc f xs)

spread :: [DOC] -> DOC
spread = folddoc (<+>)
stack :: [DOC] -> DOC
stack = folddoc (</>)

-- often a layout consists of an opening bracket, indented portion, and closing bracket...
bracket :: String -> DOC -> String -> DOC
bracket l x r = group' (TEXT l :<> NEST 2 (LINE :<> x) :<> LINE :<> TEXT r)

{- conversion of showTree' and showTrees' to use DOC (if we had redefined constructors, no
   conversion would be required)                                                        -}
showTree2 :: Tree -> DOC
showTree2 (Node s ts) = TEXT s :<> (showBracket2 ts)
showTrees2 :: [Tree] -> DOC
showTrees2 [] = NIL
showTrees2 [t] = showTree2 t
showTrees2 (t:ts) = showTree2 t :<> TEXT "," :<> LINE :<> showTrees2 ts
-- function abbreviating the second tree layout function
showBracket2 ts = bracket "[" (showTrees2 ts) "]"

-- fillwords takes a string and returns a doc filling each line with as many words as fit...
(<+/>) :: DOC -> DOC -> DOC
x <+/> y = x :<> (TEXT " " :<|> LINE) :<> y
{-  we do not expose :<|> to the user, but it is safe to expose <+/> since it satisfies the
    required invariants for unions: both TEXT " " and LINE flatten' to the same layout and the
    former has a longer first line than the latter.                                        -}
fillwords :: String -> DOC
fillwords = folddoc (<+/>) . map TEXT . words
{-  A variant of fillwords is "fill", which collapses a list of documents into a single document,
    putting a space between documents when this leads to reasonable layout, or a newline otherwise.
    This function is taken from Peyton-Jones' expansion of Hughes' library                        -}
fill :: [DOC] -> DOC
fill [] = NIL
fill [x] = x
fill (x:y:zs) = (flatten' x <+> fill (flatten' y : zs))
                :<|> (x </> fill (y:zs))
{-  note the use of flatten' in the first case of the union, to ensure that a space is only inserted
    between documents which occupy a single line.
    Also note that the invariants for union are again fulfilled.                                    -}

-- Pretty-Printing Simplified XML.
-- -------------------------------
data XML = Elt String [Att] [XML]
         | Txt String
         deriving (Show)
data Att = Att String String deriving (Show)
-- pretty-printing functions...
showXML :: XML -> DOC
showXML x = folddoc (:<>) (showXMLs x)
showXMLs :: XML -> [DOC]
showXMLs (Elt n a []) = [TEXT "<" :<> showTag n a :<> TEXT "/>"]
showXMLs (Elt n a c) = [TEXT "<" :<> showTag n a :<> TEXT ">" :<>
                        showFill showXMLs c :<>
                        TEXT "</" :<> TEXT n :<> TEXT ">"]
showXMLs (Txt s) = map TEXT (words s)
showAtts :: Att -> [DOC]
showAtts (Att n v) = [TEXT n :<> TEXT "=" :<> TEXT (quoted v)]
quoted :: String -> String
quoted s = "\"" ++ s ++ "\""
showTag :: String -> [Att] -> DOC
showTag n a = TEXT n :<> showFill showAtts a
showFill :: (t -> [DOC]) -> [t] -> DOC
showFill f [] = NIL
showFill f xs = bracket "" (fill (concat (map f xs))) ""

exXml = Elt "p" [
         Att "color" "red",
         Att "font" "Times",
         Att "size" "10"
        ] [
         Txt "Here is some",
         Elt "em" [] [
                  Txt "emphasized"
                 ],
         Txt "text.",
         Txt "Here is a",
         Elt "a" [
          Att "href" "http://www.eg.com/"
         ] [
          Txt "link"
         ],
         Txt "elsewhere."
        ]
testXml w = putStrLn $ pretty' w $ showXML exXml
{-  Note that embedded markup is either flattened of gets a line to itself.
    If we hadn't used the two occurrences of flatten' in "fill", we would
    end up with layouts where start or end tags are crammed together with
    preceding/succeeding elements rather than having  lines to themselves.    -}
