module Calculator.AST where

data Exp =
      EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EInt Integer
