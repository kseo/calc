module Calculator.Interpreter
    ( evalString
    , EvalError(..)
    ) where

import Data.Bifunctor
import Calculator.AST
import Calculator.Parser
import Control.Monad.Except

data EvalError = DivideByZero
               | ParseError
                 deriving Show

eval :: Exp -> Either EvalError Integer
eval (EAdd exp1 exp2) = do
  v1 <- eval exp1
  v2 <- eval exp2
  return $ v1 + v2
eval (ESub exp1 exp2) = do
  v1 <- eval exp1
  v2 <- eval exp2
  return $ v1 - v2
eval (EMul exp1 exp2) = do
  v1 <- eval exp1
  v2 <- eval exp2
  return $ v1 * v2
eval (EDiv exp1 exp2) = do
  dividend <- eval exp1
  divisor <- eval exp2
  if divisor == 0
    then throwError DivideByZero
    else return $ dividend `div` divisor
eval (EInt n) = Right n

evalString :: String -> Either EvalError Integer
evalString s = do
  exp <- first (const ParseError) $ parseExp s
  eval exp
