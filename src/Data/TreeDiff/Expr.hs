{-# LANGUAGE ScopedTypeVariables #-}
-- | This module uses 'Expr' for richer diffs than based on 'Tree'.
module Data.TreeDiff.Expr (
    -- * Types
    Expr (..),
    ConstructorName,
    FieldName,
    EditExpr (..),
    Edit (..),
    -- * Functions
    exprDiff,
    -- * Pretty printing
    Pretty (..),
    prettyPretty,
    prettyExpr,
    prettyEditExpr,
    ppExpr,
    ppEditExpr,
    ) where

import Data.Map           (Map)
import Data.TreeDiff.List

import qualified Data.Map         as Map
import qualified Text.PrettyPrint as PP

type ConstructorName = String
type FieldName       = String

-- | An expression tree.
data Expr
    = App ConstructorName [Expr]
    | Rec ConstructorName (Map FieldName Expr)
    | Lst [Expr]
  deriving (Eq, Show)

-- | Diff two 'Expr'.
--
-- For examples see 'ediff' in "Data.TreeDiff.Class".
exprDiff :: Expr -> Expr -> Edit EditExpr
exprDiff = impl
  where
    impl ea eb | ea == eb = Cpy (EditExp ea)

    impl ea@(App a as) eb@(App b bs)
        | a == b = Cpy $ EditApp a (map recurse (diffBy (==) as bs))
        | otherwise = Swp (EditExp ea) (EditExp eb)
    impl ea@(Rec a as) eb@(Rec b bs)
        | a == b = Cpy $ EditRec a $ Map.unions [inter, onlyA, onlyB]
        | otherwise = Swp (EditExp ea) (EditExp eb)
      where
        inter = Map.intersectionWith exprDiff as bs
        onlyA = fmap (Cpy . EditExp) (Map.difference as inter)
        onlyB = fmap (Cpy . EditExp) (Map.difference bs inter)
    impl (Lst as) (Lst bs) =
        Cpy $ EditLst (map recurse (diffBy (==) as bs))

    -- If higher level doesn't match, just swap.
    impl a b = Swp (EditExp a) (EditExp b)

    recurse (Ins x)   = Ins (EditExp x)
    recurse (Del y)   = Del (EditExp y)
    recurse (Cpy z)   = Cpy (EditExp z)
    recurse (Swp x y) = impl x y

data EditExpr
    = EditApp ConstructorName [Edit EditExpr]
    | EditRec ConstructorName (Map FieldName (Edit EditExpr))
    | EditLst [Edit EditExpr]
    | EditExp Expr  -- ^ unchanged tree
  deriving Show

-- | Because we don't want to commit to single pretty printing library,
-- we use explicit dictionary.
data Pretty doc = Pretty
    { ppCon        :: ConstructorName -> doc
    , ppRec        :: [(FieldName, doc)] -> doc
    , ppLst        :: [doc] -> doc
    , ppCpy        :: doc -> doc
    , ppIns        :: doc -> doc
    , ppDel        :: doc -> doc
    , ppSep        :: [doc] -> doc
    , ppParens     :: doc -> doc
    , ppHang       :: doc -> doc -> doc
    }


ppExpr :: Pretty doc -> Expr -> doc
ppExpr p = ppExpr' p False

ppExpr' :: forall doc. Pretty doc -> Bool -> Expr -> doc
ppExpr' p = impl where
    impl _ (App x []) = ppCon p x
    impl b (App x xs) = ppParens' b $ ppHang p (ppCon p x) $
        ppSep p $ map (impl True) xs
    impl _ (Rec x xs) = ppHang p (ppCon p x) $ ppRec p $
        map ppField' $ Map.toList xs
    impl _ (Lst xs)   = ppLst p (map (impl False) xs)

    ppField' (n, e) = (n, impl False e)

    ppParens' :: Bool -> doc -> doc
    ppParens' True  = ppParens p
    ppParens' False = id

ppEditExpr :: forall doc. Pretty doc -> Edit EditExpr -> doc
ppEditExpr p = ppSep p . ppEdit False
  where
    ppEdit :: Bool -> Edit EditExpr -> [doc]
    ppEdit b (Cpy (EditExp expr)) = [ ppCpy p $ ppExpr' p b expr ]
    ppEdit b (Cpy expr) = [ ppEExpr b expr ]
    ppEdit b (Ins expr) = [ ppIns p (ppEExpr b expr) ]
    ppEdit b (Del expr) = [ ppDel p (ppEExpr b expr) ]
    ppEdit b (Swp x y) =
        [ ppDel p (ppEExpr b x)
        , ppIns p (ppEExpr b y)
        ]

    ppEExpr :: Bool -> EditExpr -> doc
    ppEExpr _ (EditApp x []) = ppCon p x
    ppEExpr b (EditApp x xs) = ppParens' b $ ppHang p (ppCon p x) $
        ppSep p $ concatMap (ppEdit True) xs
    ppEExpr _ (EditRec x xs) = ppHang p (ppCon p x) $ ppRec p $
        map ppField' $ Map.toList xs
    ppEExpr _ (EditLst xs)   = ppLst p (concatMap (ppEdit False) xs)
    ppEExpr b (EditExp x)    = ppExpr' p b x

    ppField' (n, e) = (n, ppSep p $ ppEdit False e)

    ppParens' :: Bool -> doc -> doc
    ppParens' True  = ppParens p
    ppParens' False = id

-- | 'Pretty' via @pretty@ library.
prettyPretty :: Pretty PP.Doc
prettyPretty = Pretty
    { ppCon    = PP.text
    , ppRec    = PP.braces . PP.sep . PP.punctuate PP.comma
               . map (\(fn, d) -> PP.text fn PP.<+> PP.equals PP.<+> d)
    , ppLst    = PP.brackets . PP.sep . PP.punctuate PP.comma
    , ppCpy    = id
    , ppIns    = \d -> PP.char '+' PP.<> d
    , ppDel    = \d -> PP.char '-' PP.<> d
    , ppSep    = PP.sep
    , ppParens = PP.parens
    , ppHang   = \d1 d2 -> PP.hang d1 2 d2
    }

-- | Pretty print 'Expr' using @pretty@.
prettyExpr :: Expr -> PP.Doc
prettyExpr = ppExpr prettyPretty

-- | Pretty print @'Edit EditExpr'@ using @pretty@.
prettyEditExpr :: Edit EditExpr -> PP.Doc
prettyEditExpr = ppEditExpr prettyPretty
