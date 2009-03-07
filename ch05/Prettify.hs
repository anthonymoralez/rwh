module Prettify where

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
                Text s       -> s ++ best (col + length s) ds
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
           where len = width flatDoc
                 flatDoc = flatten doc
width :: Doc -> Int 
width (Char _)       = 1
width (Text s)       = length s
width (a `Concat` b) = (width a) + (width b)
width (a `Union` b)  = (width a) `min` (width b)
width _              = 0

 -}

spaces :: Int -> Doc
spaces n = Text $ replicate n ' '

fill :: Int -> Doc -> Doc 
fill width d = snd $ walk width 0 d

walk :: Int -> Int -> Doc -> (Int, Doc)
walk _ col Empty       = (col, Empty)
walk _ col c@(Char _)  = (col + 1, c)
walk _ col t@(Text s)  = (col + (length s), t)
walk w col (a `Concat` (_ `Union` Line)) = ((max len w), a'' `Concat` Line)
                                    where (len, a') = walk w 0 a
                                          a'' = (spaces (w - len)) `Concat` a' 
walk w col (a `Concat` b) = let (col',  a') = walk w col a
                                (col'', b') = walk w col' b
                                in (col'', a' `Concat` b')
walk w col (a `Union` _) = (len, a)
                         where (len, _) = walk w col a
walk _ _ Line = (0, Line)

val = (Char '{') </> (Text "123456789") </> (Char '}')

