module Scribe.Interpreter (Env, Value(..), showValue, run,
                           result, result1, valueObj, snakObj, exprVal) where

import PGF2
import Scribe.AbsSyn
import Text.JSON
import Text.Html
import Data.List
import qualified Data.Map as Map

type Qid = String

type Env = [(Ident,Value)]

data Value
  = Entity [(Qid,JSObject JSValue)]
  | Value  [JSObject JSValue]
  | Snak   [JSObject JSValue]
  | SValue [String]
  | QValue [Double]
  | IValue [Integer]
  | EValue [(Expr,String)]
  | HTMLValue [Html]

showValue (Entity objs) = showLines (\(_,obj) -> showJSObject obj) objs
showValue (Value  objs) = showLines showJSObject objs
showValue (Snak   objs) = showLines showJSObject objs
showValue (SValue vals) = showLines showString vals
showValue (QValue vals) = showLines shows vals
showValue (IValue vals) = showLines shows vals
showValue (EValue vals) = showLines (\(e,_) s -> showExpr [] e ++ s) vals
showValue (HTMLValue vals) = showLines (showTags (renderHtml' 0) . getHtmlElements) vals

showLines show []     = id
showLines show (x:xs) = show x . showChar '\n' . showLines show xs

showTags show []     = id
showTags show (x:xs) = show x . showLines show xs

run env db gr cnc (Tag tag attrs body) args = do
  attrs <- mapM evalAttr attrs
  body  <- run env db gr cnc body []
  return (HTMLValue [Html [HtmlTag tag attrs body]
                              | attrs <- sequence attrs
                              , body <- htmlVal cnc body])
  where
    evalAttr (name,term) = do
      val <- run env db gr cnc term []
      return [HtmlAttr name val | val <- strVal cnc val]
run env db gr cnc (Var var) args =
  case Map.lookup var env of
    Just val -> val db gr args
    Nothing  -> case functionType gr var of
                  Just (DTyp _ cat _) -> return (EValue [(EFun var,cat)])
                  Nothing             -> fail ("Undefined variable "++var)
run env db gr cnc Empty args =
  return (HTMLValue [noHtml])
run env db gr cnc (C t1 t2) args = do
  v1 <- run env db gr cnc t1 []
  v2 <- run env db gr cnc t2 []
  return (HTMLValue [html1 +++ html2 | html1 <- htmlVal cnc v1
                                     , html2 <- htmlVal cnc v2])
run env db gr cnc (Delim t) args = do
  v <- run env db gr cnc t []
  return (HTMLValue [concatHtml (intersperse (Html [HtmlString " "]) (htmlVal cnc v))])
run env db gr cnc (Prop t prop) [] = do
  Entity objs <- run env db gr cnc t []
  return (Value [x | (_,obj) <- objs,
                     x <- result (valFromObj prop obj)])
run env db gr cnc (Qual t prop) [] = do
  Value objs <- run env db gr cnc t []
  return (Snak [x | obj <- objs,
                    x <- result (valFromObj "qualifiers" obj >>= valFromObj prop)])
run env db gr cnc (App f x) args = do
  x <- run env db gr cnc x []
  run env db gr cnc f (x:args)
run env db gr cnc (Lit (LStr s)) args = do
  return (SValue [s])
run env db gr cnc (Lit (LFlt f)) args = do
  return (QValue [f])
run env db gr cnc (Lit (LInt f)) args = do
  return (IValue [f])

result (Ok xs)   = xs
result (Error _) = []

result1 (Ok x)  = [x]
result1 (Error _) = []

valueObj (Value objs) = objs

snakObj (Value objs) = [x | obj <- objs,
                            x <- result1 (valFromObj "mainsnak" obj)]
snakObj (Snak objs)  = objs

exprVal (EValue es) = es

htmlVal cnc (HTMLValue htmls) = htmls
htmlVal cnc (EValue es) = [Html [HtmlString (linearize cnc e)] | (e,_) <- es]

strVal cnc (Entity objs) = map (\(_,obj) -> showJSObject obj "") objs
strVal cnc (Value  objs) = map (\obj -> showJSObject obj "") objs
strVal cnc (Snak   objs) = map (\obj -> showJSObject obj "") objs
strVal cnc (SValue vals) = vals
strVal cnc (QValue vals) = map show vals
strVal cnc (IValue vals) = map show vals
strVal cnc (EValue vals) = map (\(e,_) -> linearize cnc e) vals
strVal cnc (HTMLValue vals) = map renderHtml vals
