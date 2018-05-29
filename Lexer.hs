module Lexer where

import Data.Char
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Control.Applicative (some)

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token (integer,float,parens,whiteSpace,makeTokenParser)

import Text.Parsec.Language (emptyDef)

-- partially adapted from Write You a Haskell by Stephen Diehl.

-- NOTE: definitely not made optimally.
--data Prim = I String | F String | Ident String | OpIdent String deriving (Show,Eq)
data Expr = Mon Expr Expr | Dyad Expr Expr Expr
          | Tup [Expr]
          | Arr [Expr] 
          | I String | F String | Ident String | OpIdent String
          | Fun String | ParseErr deriving (Show,Eq)

lexer = makeTokenParser emptyDef

integerp :: Parser Expr
integerp = do
  whiteSpace lexer
  s <- string "_" <|> return []
  cs <- some digit
  whiteSpace lexer
  return $ I $ s++cs

-- TODO: add notation for exponents.
int_floatp :: Parser Expr
int_floatp = do
  whiteSpace lexer
  s <- string "_" <|> return []
  cs <- some digit
  dec <- optionMaybe $ string "."
  q <- case dec of (Just a) -> do { ds <- some digit; return $ F $ s++cs++a++ds; }
                   Nothing  -> do { return $ I $ s++cs; }
  whiteSpace lexer
  return q

listp :: Parser Expr -> Parser Expr
listp q = do
  whiteSpace lexer
  s <- q `sepBy1` (whiteSpace lexer)
  whiteSpace lexer
  return $ if length s == 1 then head s else Arr s

identp :: Parser Expr
identp = do
  s <- letter
  cs <- many $ alphaNum <|> oneOf "_"
  return $ Ident $ [s]++cs

op_ident_parser :: ParsecT String u Data.Functor.Identity.Identity String
op_ident_parser = foldr1 (<|>) $ map string ["+","-","*","/","^","<=",">=","<",">","=","=:"]

-- NOTE: there should be a shorter way of representing this.
op_identp :: Parser Expr
op_identp = do
  s <- op_ident_parser
  return $ OpIdent s

funp :: Parser Expr
funp = do
  s <- char '\''
  f <- (some $ alphaNum <|> oneOf "_") <|> op_ident_parser
  return $ Fun f

expr :: Parser Expr
expr = (do { q <- mon; e <- expr; return $ q e; }) <|> factor `chainr1` dyad

dyad :: Parser (Expr -> Expr -> Expr)
dyad = do
  q <- op_identp <|> identp
  return $ Dyad q
mon :: Parser (Expr -> Expr)
mon = do
  q <- op_identp <|> identp
  return $ Mon q

factor :: Parser Expr
factor = listp int_floatp <|> listp funp <|> parens lexer expr

run :: String -> Expr
run = either (\_ -> ParseErr) id . runParser expr () "out"
