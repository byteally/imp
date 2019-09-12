{-# LANGUAGE DeriveGeneric #-}
module Language.Imp.Syntax where

import GHC.Generics
import Data.Text

data CompUnit = CompUnit [Function]
              deriving (Show, Generic)

data Function = Function Ident Params Block
              deriving (Show, Generic)

data Block    = Block [Statement]
              deriving (Show, Generic)

data Statement =
    StatementExpr Expr
  | Return        Expr
  | If            Expr  Block Block
  | Loop          Expr  Block
  | Declaration   Declaration
  | Assignment    Assignment
  deriving (Show, Generic)

data Declaration = Decl Ident (Maybe Expr)
                 deriving (Show, Generic)

data Assignment  = Assn Ident Expr
                 deriving (Show, Generic)

data Expr =
    FunctionCall Ident [Expr]
  | Var          Ident
  | Lit          Literal
  deriving (Show, Generic)

type Params = [Ident]

newtype Ident = Ident { getIdent :: Text }
              deriving (Show, Generic)

data Literal = Integer   Int
             | Character Char
             | String    Text
             deriving (Show, Generic)
