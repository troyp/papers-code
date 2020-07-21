import Control.Applicative

f1 :: [x] -> x
f1 = head
f1a a = a . head
f1b a = head . a

f2 :: [x] -> [x]
f2 = tail
f2a a = map a . tail
f2b a = tail . map a
