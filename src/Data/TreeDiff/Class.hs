{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TreeDiff.Class (
    ediff,
    ToExpr (..),
    defaultExprViaShow
    ) where

import Data.TreeDiff.Expr
import GHC.Generics (Generic)

import qualified Data.Map as Map
import Generics.SOP
    ( SOP (..), I (..), K (..), NP (..)
    , All, All2
    , DatatypeInfo (..), constructorInfo, ConstructorInfo (..), FieldInfo (..)
    , hcmap, hcliftA2, hcollapse, mapIK
    )
import Generics.SOP.GGP
    ( GFrom, GCode, GDatatypeInfo, GCode, gfrom, gdatatypeInfo
    )
import Data.Proxy (Proxy (..))

#ifdef __DOCTEST__
import qualified Text.PrettyPrint as PP
#endif

-- | Difference between two 'ToExpr' values.
--
-- >>> let x = (1, Just 2) :: (Int, Maybe Int)
-- >>> let y = (1, Nothing)
-- >>> ppEditExpr prettyP (ediff x y)
-- ((,) 1 -(Just 2) +Nothing)
--
-- >>> data Foo = Foo { fooInt :: Int, fooBool :: [Bool], fooString :: String } deriving (Eq, Generic)
-- >>> instance ToExpr Foo
--
-- >>> ppEditExpr prettyP $ ediff (Foo 2 [True] "fo") (Foo 3 [True] "fo")
-- Foo {fooBool = [True], fooInt = -2 +3, fooString = "fo"}
--
-- >>> ppEditExpr prettyP $ ediff (Foo 42 [True, False] "old") (Foo 42 [False, False, True] "new")
-- Foo
--   {fooBool = [-True, +False, False, +True],
--    fooInt = 42,
--    fooString = -"old" +"new"}
--
ediff :: (ToExpr a, Eq a) => a -> a -> Edit EditExpr
ediff x y = exprDiff (toExpr x) (toExpr y)

-- |
--
-- >>> toExpr ((1, Just 2) :: (Int, Maybe Int))
-- App "(,)" [App "1" [],App "Just" [App "2" []]]
--
class ToExpr a where
    toExpr :: a -> Expr
    default toExpr
        :: (Generic a, All2 ToExpr (GCode a), GFrom a, GDatatypeInfo a)
        => a -> Expr
    toExpr x = sopToExpr (gdatatypeInfo (Proxy :: Proxy a)) (gfrom x)

    listToExpr :: [a] -> Expr
    listToExpr = Lst . map toExpr

instance ToExpr Expr where
    toExpr = id

defaultExprViaShow :: Show a => a -> Expr
defaultExprViaShow x = App (show x) []

sopToExpr :: All2 ToExpr xss => DatatypeInfo xss -> SOP I xss -> Expr
sopToExpr di (SOP xss) = hcollapse $ hcliftA2
    (Proxy :: Proxy (All ToExpr))
    (\ci xs -> K (sopNPToExpr ci xs))
    (constructorInfo di)
    xss

sopNPToExpr :: All ToExpr xs => ConstructorInfo xs -> NP I xs -> Expr
sopNPToExpr (Infix cn _ _) xs = App cn $ hcollapse $
    hcmap (Proxy :: Proxy ToExpr) (mapIK toExpr) xs
sopNPToExpr (Constructor cn) xs = App cn $ hcollapse $
    hcmap (Proxy :: Proxy ToExpr) (mapIK toExpr) xs
sopNPToExpr (Record cn fi) xs = Rec cn $ Map.fromList $ hcollapse $
    hcliftA2 (Proxy :: Proxy ToExpr) mk fi xs
  where
    mk :: ToExpr x => FieldInfo x -> I x -> K (FieldName, Expr) x
    mk (FieldInfo fn) (I x) = K (fn, toExpr x)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance ToExpr Int where toExpr = defaultExprViaShow
instance ToExpr Bool where toExpr = defaultExprViaShow

instance ToExpr Char where
    toExpr = defaultExprViaShow
    listToExpr = defaultExprViaShow

instance ToExpr a => ToExpr (Maybe a) where
    toExpr Nothing  = App "Nothing" []
    toExpr (Just x) = App "Just" [toExpr x]

instance (ToExpr a, ToExpr b) => ToExpr (Either a b) where
    toExpr (Left x)  = App "Left"  [toExpr x]
    toExpr (Right y) = App "Right" [toExpr y]

instance ToExpr a => ToExpr [a] where
    toExpr = listToExpr

instance (ToExpr a, ToExpr b) => ToExpr (a, b) where
    toExpr (x, y) = App "(,)" [toExpr x, toExpr y]

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

#ifdef __DOCTEST__
prettyP :: Pretty PP.Doc
prettyP = Pretty
    { ppCon    = PP.text
    , ppRec    = PP.braces . PP.sep . PP.punctuate PP.comma
               . map (\(fn, d) -> PP.text fn PP.<+> PP.equals PP.<+> d)
    , ppLst    = PP.brackets . PP.sep . PP.punctuate PP.comma
    , ppIns    = \d -> PP.char '+' PP.<> d
    , ppDel    = \d -> PP.char '-' PP.<> d
    , ppSep    = PP.sep
    , ppParens = PP.parens
    , ppHang   = \d1 d2 -> PP.hang d1 2 d2
    }

-- $setup
-- >>> :set -XDeriveGeneric
#endif
