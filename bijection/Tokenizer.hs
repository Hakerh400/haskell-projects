module Tokenizer
  ( Token(..)
  , tokenize
  ) where

import Data.Char
import Base

data Token
  = Eof
  | Ident String
  | EqSign
  | OpenParen
  | ClosedParen
  | EndOfDef
  | Illegal String
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize "" = [EndOfDef, Eof]
tokenize (' ':s) = tokenize s
tokenize ('\t':s) = tokenize s
tokenize ('\r':'\n':s) = tokenizeNewLine s
tokenize ('\r':s) = tokenizeNewLine s
tokenize ('\n':s) = tokenizeNewLine s
tokenize ('=':s) = EqSign : tokenize s
tokenize ('(':s) = OpenParen : tokenize s
tokenize (')':s) = ClosedParen : tokenize s
tokenize s = tokenizeIdent s

tokenizeIdent :: String -> [Token]
tokenizeIdent s@(c:_) = if isFstIdentChar c
  then let
    (name, rest) = split isIdentChar s
    in Ident name : tokenize rest
  else [Illegal s]

tokenizeNewLine :: String -> [Token]
tokenizeNewLine s@(c:_) = if c `elem` " \t"
  then tokenize s
  else EndOfDef : tokenize s
tokenizeNewLine s = tokenize s

isFstIdentChar :: Char -> Bool
isFstIdentChar = isLetter

isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || isDigit c || c == '\x5F'

split :: (a -> Bool) -> [a] -> ([a], [a])
split f xs = break (not . f) xs