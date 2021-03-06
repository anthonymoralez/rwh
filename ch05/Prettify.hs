module Prettify where

import Prelude hiding (length)
import qualified Prelude(length)

data Doc = Empty 
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

line :: Doc
line = Line 

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y 

char :: Char -> Doc
char c = Char c

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

concat :: [[a]] -> [a]
concat = foldr (++) []

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) = 
              case d of 
                Empty        -> best col ds
                Char c       -> c : best (col + 1) ds
                Text s       -> s ++ best (col + Prelude.length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: Int -> String -> Bool 
w `fits` _ | w < 0 = False
_ `fits` ""        = True
_ `fits` ('\n':_)  = True
w `fits` (_:cs)    = (w - 1) `fits` cs

{- Exercise 1. Write a function, fill, with the following type signature.
 - It should add spaces to a document until it is the given number of columns 
 - wide. If it is already wider than this value, it should add no spaces.
fill :: Int -> Doc -> Doc
fill w doc | w > len  = text (replicate (w - len) ' ') <> flatDoc
           | otherwise = flatDoc
           where len = length flatDoc
                 flatDoc = flatten doc
 -}

fill :: Int -> Doc -> Doc 
fill width doc = fold joinLines (linesDoc doc)
                   where joinLines a b =  (insertSpaces a) <> (insertSpaces b)
                         insertSpaces doc = (spaces (width - (length doc))) <> doc 

linesDoc :: Doc -> [Doc]
linesDoc doc = foldr concatLines [] (toList doc)  
    where concatLines :: Doc -> [Doc] -> [Doc]
          concatLines l@(Line) docs = l : docs 
          concatLines u@(Union _ Line) docs = u : docs 
          concatLines doc (d:ds) = (Concat doc d) : ds
          concatLines doc [] = [doc]

toList :: Doc -> [Doc] 
toList (Concat a b) = (toList a) ++ (toList b)
toList doc = [doc]

length :: Doc -> Int 
length (Char _)       = 1
length (Text s)       = Prelude.length s
length (a `Concat` b) = (length a) + (length b)
length (a `Union` b)  = (length a) `min` (length b)
length _              = 0

spaces :: Int -> Doc
spaces n = Text $ replicate n ' '


{- Exercise 2.
 - Our pretty printer does not take nesting into account. Whenever we open 
 - parentheses, braces, or brackets, any lines that follow should be indented so
 - that they are aligned with the opening character until a matching closing 
 - character is encountered. Add support for nesting, with a controllable amount 
 - of indentation.
 -}

nest :: Int -> Doc -> Doc
nest widthOfIndent doc = indent 0 (linesDoc doc)
    where indent :: Int -> [Doc] -> Doc
          indent _ [] = empty
          indent level (d:ds) = ((spaces (widthOfIndent * level)) <> d) <> indent level' ds
                                where level' = level + (indentDelta (toList d))

indentDelta :: [Doc] -> Int
indentDelta = foldr countBrackets 0  
    where countBrackets (Char c) i | openIndentChar c  = i+1
                                   | closeIndentChar c = i-1
                                   | otherwise         = i
          countBrackets _ i = i

openIndentChar, closeIndentChar :: Char -> Bool
openIndentChar = flip elem "([{" 
closeIndentChar = flip elem ")]}" 
