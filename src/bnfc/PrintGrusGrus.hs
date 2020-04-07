{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintGrusGrus.
--   Generated by the BNF converter.

module PrintGrusGrus where

import qualified AbsGrusGrus
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsGrusGrus.Ident where
  prt _ (AbsGrusGrus.Ident i) = doc (showString i)

instance Print AbsGrusGrus.Body where
  prt i e = case e of
    AbsGrusGrus.Body decls exp -> prPrec i 0 (concatD [prt 0 decls, prt 0 exp])

instance Print [AbsGrusGrus.Decl] where
  prt = prtList

instance Print AbsGrusGrus.Decl where
  prt i e = case e of
    AbsGrusGrus.DPut exp -> prPrec i 0 (concatD [doc (showString "put"), prt 0 exp, doc (showString ";")])
    AbsGrusGrus.DVal typedident exp -> prPrec i 0 (concatD [doc (showString "val"), prt 0 typedident, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsGrusGrus.DFun id typedidents type_ body -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 id, doc (showString "("), prt 0 typedidents, doc (showString ")"), doc (showString "->"), prt 0 type_, doc (showString "{"), prt 0 body, doc (showString "}")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsGrusGrus.TypedIdent where
  prt i e = case e of
    AbsGrusGrus.TypedIdent id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 type_])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsGrusGrus.TypedIdent] where
  prt = prtList

instance Print AbsGrusGrus.Exp where
  prt i e = case e of
    AbsGrusGrus.EIfte exp1 exp2 exp3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp1, doc (showString "then"), prt 0 exp2, doc (showString "else"), prt 0 exp3])
    AbsGrusGrus.EOr exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "||"), prt 3 exp2])
    AbsGrusGrus.EAnd exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "&&"), prt 4 exp2])
    AbsGrusGrus.EEq exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "=="), prt 5 exp2])
    AbsGrusGrus.ENeq exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "!="), prt 5 exp2])
    AbsGrusGrus.ELt exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "<"), prt 6 exp2])
    AbsGrusGrus.EGt exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString ">"), prt 6 exp2])
    AbsGrusGrus.ELe exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "<="), prt 6 exp2])
    AbsGrusGrus.EGe exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString ">="), prt 6 exp2])
    AbsGrusGrus.EAdd exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "+"), prt 7 exp2])
    AbsGrusGrus.ESub exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "-"), prt 7 exp2])
    AbsGrusGrus.EMult exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "*"), prt 8 exp2])
    AbsGrusGrus.EDiv exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "/"), prt 8 exp2])
    AbsGrusGrus.EMod exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "%"), prt 8 exp2])
    AbsGrusGrus.ENot exp -> prPrec i 8 (concatD [doc (showString "!"), prt 9 exp])
    AbsGrusGrus.ECallIdent id exps -> prPrec i 9 (concatD [prt 0 id, doc (showString "("), prt 0 exps, doc (showString ")")])
    AbsGrusGrus.ECallExp exp exps -> prPrec i 9 (concatD [prt 0 exp, doc (showString "<~"), doc (showString "("), prt 0 exps, doc (showString ")")])
    AbsGrusGrus.ELambda typedidents body -> prPrec i 9 (concatD [doc (showString "("), doc (showString "\\"), prt 0 typedidents, doc (showString "~>"), prt 0 body, doc (showString ")")])
    AbsGrusGrus.EInt n -> prPrec i 9 (concatD [prt 0 n])
    AbsGrusGrus.EBool boolean -> prPrec i 9 (concatD [prt 0 boolean])
    AbsGrusGrus.EUnit unit -> prPrec i 9 (concatD [prt 0 unit])
    AbsGrusGrus.EVar id -> prPrec i 9 (concatD [prt 0 id])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsGrusGrus.Exp] where
  prt = prtList

instance Print AbsGrusGrus.Boolean where
  prt i e = case e of
    AbsGrusGrus.BTrue -> prPrec i 0 (concatD [doc (showString "True")])
    AbsGrusGrus.BFalse -> prPrec i 0 (concatD [doc (showString "False")])

instance Print AbsGrusGrus.Unit where
  prt i e = case e of
    AbsGrusGrus.Unit -> prPrec i 0 (concatD [doc (showString "Unit")])

instance Print AbsGrusGrus.Type where
  prt i e = case e of
    AbsGrusGrus.TInt -> prPrec i 0 (concatD [doc (showString "Int")])
    AbsGrusGrus.TBool -> prPrec i 0 (concatD [doc (showString "Bool")])
    AbsGrusGrus.TArrow type_1 type_2 -> prPrec i 0 (concatD [prt 0 type_1, doc (showString "->"), prt 0 type_2])

