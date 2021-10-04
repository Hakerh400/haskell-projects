module Parser
  ( IdentDef(..)
  , ParsedExpr(..)
  , parse
  ) where

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
  , _expr :: ParsedExpr
  } deriving (Show)

data ParsedExpr
  = Ident String
  | Call ParsedExpr ParsedExpr
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
  -- TK.Ident name <- getToken
  tok <- getToken
  case tok of
    EndOfDef -> parseDef
    TK.Ident name -> do
      args <- parseArgs
      EqSign <- getToken
      expr <- parseExpr True
      return $ IdentDef
        { _name = name
        , _args = args
        , _expr = expr
        }

parseArgs :: Parser [String]
parseArgs = do
  tok <- queryToken
  case tok of
    TK.Ident name -> do
      getToken
      args <- parseArgs
      
      if name `elem` args
        then error $ concat ["Duplicate argument ", show name]
        else pure ()
      
      return $ name : args
    _ -> return []

parseExpr :: Bool -> Parser ParsedExpr
parseExpr top = do
  terms <- parseExprTerms top
  return $ foldl1 Call terms

parseExprTerms :: Bool -> Parser [ParsedExpr]
parseExprTerms top = do
  tok <- getToken
  case (top, tok) of
    (True, EndOfDef) -> return []
    (False, ClosedParen) -> return []
    (_, TK.Ident name) -> do
      terms <- parseExprTerms top
      return $ Ident name : terms
    (_, OpenParen) -> do
      expr <- parseExpr False
      terms <- parseExprTerms top
      return $ expr : terms
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