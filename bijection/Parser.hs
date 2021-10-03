module Parser
  ( IdentDef(..)
  , ParsedBody(..)
  , parse
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import Base
import Tokenizer hiding (Ident)
import qualified Tokenizer as TK

type Parser = State ParserState

data ParserState = ParserState
  { _toks :: [Token]
  , _defs :: [IdentDef]
  }

data IdentDef = IdentDef
  { _name :: String
  , _args :: [String]
  , _body :: ParsedBody
  } deriving (Show)

data ParsedBody
  = Ident String
  | Call ParsedBody ParsedBody
  deriving (Show)

parse :: String -> [IdentDef]
parse str = let
  toks = tokenize str
  st = execState parseProg $ initState toks
  in reverse $ _defs st

initState :: [Token] -> ParserState
initState toks = ParserState
  { _toks = toks
  , _defs = []
  }

parseProg :: Parser ()
parseProg = do
  tok <- queryToken
  if tok == Eof
    then pure ()
    else do
      def <- parseDef
      modify $ \s -> s {_defs = def : _defs s}
      parseProg

parseDef :: Parser IdentDef
parseDef = do
  TK.Ident name <- getToken
  args <- parseArgs
  EqSign <- getToken
  body <- parseBody True
  return $ IdentDef
    { _name = name
    , _args = args
    , _body = body
    }

parseArgs :: Parser [String]
parseArgs = do
  tok <- queryToken
  case tok of
    TK.Ident name -> do
      getToken
      args <- parseArgs
      return $ name : args
    _ -> return []

parseBody :: Bool -> Parser ParsedBody
parseBody top = do
  terms <- parseBodyTerms top
  return $ foldl1 Call terms

parseBodyTerms :: Bool -> Parser [ParsedBody]
parseBodyTerms top = do
  tok <- getToken
  case (top, tok) of
    (True, EndOfDef) -> return []
    (False, ClosedParen) -> return []
    (_, TK.Ident name) -> do
      terms <- parseBodyTerms top
      return $ Ident name : terms
    (_, OpenParen) -> do
      body <- parseBody False
      terms <- parseBodyTerms top
      return $ body : terms
    -- a -> error$show$a

getOrQueryToken :: Bool -> Parser Token
getOrQueryToken b = do
  (tok:toks) <- gets _toks
  if b
    then modify $ \s -> s {_toks = toks}
    else pure ()
  return tok

getToken :: Parser Token
getToken = getOrQueryToken True

queryToken :: Parser Token
queryToken = getOrQueryToken False