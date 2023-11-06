{-# LANGUAGE MonadComprehensions #-}
module Scribe.Prelude (prelude) where

import PGF2
import Text.JSON
import Database.Daison
import qualified Data.Map as Map
import Scribe.SenseSchema
import Scribe.Interpreter

prelude = Map.fromList $
  [("mainsnak", mainsnak)
  ,("qid", qid)
  ,("time", time)
  ,("quantity", quantity)
  ,("expr", expr)
  ,("mkCN", mkCN)
  ]

mainsnak db gr [arg] = do
  return (Snak [x | obj <- valueObj arg,
                    x <- result1 (valFromObj "mainsnak" obj)])

qid db gr [Entity vals] = do
  return (SValue [qid | (qid,_) <- vals])
qid db gr [arg] = do
  return (SValue [x | obj <- snakObj arg,
                      x <- result1 (valFromObj "datavalue" obj >>= valFromObj "value" >>= valFromObj "id")])

time db gr [arg] = do
  return (SValue [x | obj <- snakObj arg,
                      x <- result1 (valFromObj "datavalue" obj >>= valFromObj "value" >>= valFromObj "time")])

quantity db gr [arg] = do
  return (QValue [quantity x | obj <- snakObj arg,
                               x <- result1 (valFromObj "datavalue" obj >>= valFromObj "value" >>= valFromObj "amount")])
  where    
    quantity ('+':s) = read s
    quantity s       = read s

expr db gr [SValue vals] = do
  return (EValue [(ELit (LStr val), "String") | val <- vals])
expr db gr [QValue vals] = do
  return (EValue [(ELit (LFlt val), "Float") | val <- vals])
expr db gr [IValue vals] = do
  return (EValue [(ELit (LInt val), "Int") | val <- vals])
expr db gr [Entity vals] = do
  es <- runDaison db ReadOnlyMode $
           select [(EFun (lex_fun lex), cat)
                       | (qid,_) <- anyOf vals,
                         (_,lex) <- fromIndex lexemes_qid (at qid),
                         Just (DTyp _ cat _) <- anyOf [functionType gr (lex_fun lex)]]
  return (EValue es)
expr db gr [arg] = do
  es <- runDaison db ReadOnlyMode $
           select [(EFun (lex_fun lex), cat)
                       | qid <- anyOf [x | obj <- snakObj arg,
                                           x <- result1 (valFromObj "datavalue" obj >>= valFromObj "value" >>= valFromObj "id")],
                         (_,lex) <- fromIndex lexemes_qid (at qid),
                         Just (DTyp _ cat _) <- anyOf [functionType gr (lex_fun lex)]]
  return (EValue es)

mkCN db gr [arg] = do
  return (EValue [(mkApp "UseN" [e], "CN") | (e,cat) <- exprVal arg])
