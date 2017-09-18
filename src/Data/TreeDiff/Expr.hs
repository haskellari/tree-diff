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
    ppExpr,
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
ppExpr p = impl where
    impl (App x []) = ppCon p x
    impl (App x xs) = ppParens p $ ppHang p (ppCon p x) $
        ppSep p $ map impl xs
    impl (Rec x xs) = ppHang p (ppCon p x) $ ppRec p $
        map ppField' $ Map.toList xs
    impl (Lst xs)   = ppLst p (map impl xs)

    ppField' (n, e) = (n, impl e)

ppEditExpr :: Pretty doc -> Edit EditExpr -> doc
ppEditExpr p = ppSep p . ppEdit
  where
    ppEdit (Cpy (EditExp expr)) = [ ppCpy p $ ppExpr p expr ]
    ppEdit (Cpy expr) = [ ppEExpr expr ]
    ppEdit (Ins expr) = [ ppIns p (ppEExpr expr) ]
    ppEdit (Del expr) = [ ppDel p (ppEExpr expr) ]
    ppEdit (Swp a b) =
        [ ppDel p (ppEExpr a)
        , ppIns p (ppEExpr b)
        ]

    ppEExpr (EditApp x []) = ppCon p x
    ppEExpr (EditApp x xs) = ppParens p $ ppHang p (ppCon p x) $
        ppSep p $ concatMap ppEdit xs
    ppEExpr (EditRec x xs) = ppHang p (ppCon p x) $ ppRec p $
        map ppField' $ Map.toList xs
    ppEExpr (EditLst xs)   = ppLst p (concatMap ppEdit xs)
    ppEExpr (EditExp x)    = ppExpr p x

    ppField' (n, e) = (n, ppSep p $ ppEdit e)
