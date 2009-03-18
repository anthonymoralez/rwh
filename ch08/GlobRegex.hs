module GlobRegex 
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))
import Data.Char (toUpper)

globToRegex :: Bool -> String -> String
globToRegex caseSensitive cs = '^' : (globToRegex' caseSensitive cs) ++ "$"

globToRegex' :: Bool -> String -> String
globToRegex' _ "" = ""

globToRegex' caseSensitive ('*':cs) = ".*" ++ globToRegex' caseSensitive cs

globToRegex' caseSensitive ('?':cs) = '.' : globToRegex' caseSensitive cs

globToRegex' caseSensitive ('[':'!':c:cs) = "[^" ++ c : toUpper c : charClass caseSensitive cs
globToRegex' caseSensitive ('[':c:cs)     = '[' : c : toUpper c : charClass caseSensitive cs
globToRegex' _ ('[':_)                    = error "unterminated character class"

globToRegex' caseSensitive (c:cs) = escape caseSensitive c ++ globToRegex' caseSensitive cs

escape :: Bool -> Char -> String
escape caseSensitive c | c `elem` regexChars = '\\' : [c]
                       | otherwise = if caseSensitive 
                                        then '[' : c : toUpper c : "]"
                                        else [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: Bool -> String -> String
charClass caseSensitive (']':cs) = ']' : globToRegex' caseSensitive cs
charClass caseSensitive (c:cs)   = c : toUpper c : charClass caseSensitive cs
charClass _ []                   = error "unterminated character class"

matchesGlob :: Bool -> FilePath -> String -> Bool
matchesGlob caseSensitive name pat = name =~ globToRegex caseSensitive pat

