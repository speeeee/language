module Lexer where

import Data.Char
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
          | I String | F String | Ident String | OpIdent String | ParseErr deriving (Show,Eq)

lexer = makeTokenParser emptyDef

integerp :: Parser Expr
integerp = do
  whiteSpace lexer
  s <- string "-" <|> return []
  cs <- some digit
  whiteSpace lexer
  return $ I $ s++cs

floatp :: Parser Expr
floatp = do
  whiteSpace lexer
  s <- string "-" <|> return []
  cs <- many digit
  dec <- string "."
  ds <- some digit
  whiteSpace lexer
  return $ F $ s++cs++dec++ds

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

-- NOTE: there should be a shorter way of representing this.
op_identp :: Parser Expr
op_identp = do
  s <- foldr1 (<|>) $ map string ["+","-","*","/","^","<=",">=","<",">","=","=:"]
  return $ OpIdent s

expr :: Parser Expr
expr = factor `chainl1` dyad

dyad = do
  q <- op_identp
  return $ Dyad q

factor :: Parser Expr
factor = listp floatp <|> listp integerp <|> parens lexer expr

run :: String -> Expr
run = either (\_ -> ParseErr) id . runParser expr () "out"
