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
    ppEditExpr,
    ) where

import Data.Map (Map)
import Data.TreeDiff.List
import qualified Data.Map as Map

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
    impl ea@(App a as) eb@(App b bs)
        | a == b = Cpy $ EditApp a (map recurse (diffBy (==) as bs))
        | otherwise = Swp (treeToEdit ea) (treeToEdit eb)
    impl ea@(Rec a as) eb@(Rec b bs)
        | a == b = Cpy $ EditRec a $ Map.unions [inter, onlyA, onlyB]
        | otherwise = Swp (treeToEdit ea) (treeToEdit eb)
      where
        inter = Map.intersectionWith exprDiff as bs
        onlyA = fmap (Cpy . treeToEdit) (Map.difference as inter)
        onlyB = fmap (Cpy . treeToEdit) (Map.difference bs inter)
    impl (Lst as) (Lst bs) =
        Cpy $ EditLst (map recurse (diffBy (==) as bs))

    -- If higher level doesn't match, just swap.
    impl a b = Swp (treeToEdit a) (treeToEdit b)

    recurse (Ins x)   = Ins (treeToEdit x)
    recurse (Del y)   = Ins (treeToEdit y)
    recurse (Cpy z)   = Cpy (treeToEdit z)
    recurse (Swp x y) = impl x y

data EditExpr
    = EditApp ConstructorName [Edit EditExpr]
    | EditRec ConstructorName (Map FieldName (Edit EditExpr))
    | EditLst [Edit EditExpr]
  deriving Show

treeToEdit :: Expr -> EditExpr
treeToEdit = go
    where
      go (App x xs) = EditApp x (fmap (Cpy . go) xs)
      go (Rec x xs) = EditRec x (fmap (Cpy . go) xs)
      go (Lst xs)   = EditLst (fmap (Cpy . go) xs)

-- | Because we don't want to commit to single pretty printing library,
-- we use explicit dictionary.
data Pretty doc = Pretty
    { ppCon        :: ConstructorName -> doc
    , ppRec        :: [(FieldName, doc)] -> doc
    , ppLst        :: [doc] -> doc
    , ppIns        :: doc -> doc
    , ppDel        :: doc -> doc
    , ppSep        :: [doc] -> doc
    , ppParens     :: doc -> doc
    , ppHang       :: doc -> doc -> doc
    }

ppEditExpr :: Pretty doc -> Edit EditExpr -> doc
ppEditExpr p = ppSep p . ppEdit
  where
    ppEdit (Cpy expr) = [ ppExpr expr ]
    ppEdit (Ins expr) = [ ppIns p (ppExpr expr) ]
    ppEdit (Del expr) = [ ppDel p (ppExpr expr) ]
    ppEdit (Swp a b) =
        [ ppIns p (ppExpr a)
        , ppDel p (ppExpr b)
        ]

    ppExpr (EditApp x []) = ppCon p x
    ppExpr (EditApp x xs) = ppParens p $ ppHang p (ppCon p x) $
        ppSep p $ concatMap ppEdit xs
    ppExpr (EditRec x xs) = ppHang p (ppCon p x) $ ppRec p $
        map ppField' $ Map.toList xs
    ppExpr (EditLst xs)   = ppLst p (concatMap ppEdit xs)

    ppField' (n, e) = (n, ppSep p $ ppEdit e)

