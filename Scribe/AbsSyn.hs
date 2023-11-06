module Scribe.AbsSyn where

import PGF2
import qualified Data.ByteString.Char8 as BS
import Text.JSON

type Ident = String

data Term
  = Tag Ident [Attribute] Term
  | App Term Term
  | Var Ident
  | C Term Term
  | Empty
  | Delim Term
  | Prop Term Ident
  | Qual Term Ident
  | Lit Literal
  deriving Show

type Attribute = (Ident,Term)
