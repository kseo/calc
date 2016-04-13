module Calculator.Parser
    ( parseExp
    ) where

import Calculator.AST

import           Control.Monad (void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String (Parser)

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme = L.lexeme sc
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer

parens :: Parser Exp -> Parser Exp
parens = between (symbol "(") (symbol ")")

term =  parens expr
    <|> EInt <$> integer
    <?> "term"

table = [ [binary "*" EMul, binary "/" EDiv]
        , [binary "+" EAdd, binary "-" ESub]
        ]

binary name f = InfixL (symbol name >> return f)

expr :: Parser Exp
expr = makeExprParser term table <?> "expression"

parseExp :: String -> Either ParseError Exp
parseExp = parse expr ""
