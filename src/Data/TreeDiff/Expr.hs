-- | This module uses 'Expr' for richer diffs than based on 'Tree'.
module Data.TreeDiff.Expr (
    -- * Types
    Expr (..),
    ConstructorName,
    FieldName,
    EditExpr (..),
    Edit (..),
    exprDiff,
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq (NFData (..))
import Data.Semialign  (alignWith)
import Data.These      (These (..))

import Data.TreeDiff.List
import Data.TreeDiff.OMap (OMap)

import qualified Data.TreeDiff.OMap as OMap
import qualified Test.QuickCheck    as QC

-- | Constructor name is a string
type ConstructorName = String
--
-- | Record field name is a string too.
type FieldName       = String

-- | A untyped Haskell-like expression.
--
-- Having richer structure than just 'Tree' allows to have richer diffs.
data Expr
    = App ConstructorName [Expr]                 -- ^ application
    | Rec ConstructorName (OMap FieldName Expr)  -- ^ record constructor
    | Lst [Expr]                                 -- ^ list constructor
  deriving (Eq, Ord, Show)

instance NFData Expr where
    rnf (App n es) = rnf n `seq` rnf es
    rnf (Rec n fs) = rnf n `seq` rnf fs
    rnf (Lst es)   = rnf es

instance QC.Arbitrary Expr where
    arbitrary = QC.scale (min 25) $ QC.sized arb where
        arb n | n <= 0 = QC.oneof
            [ (`App` []) <$> arbName
            ,  (`Rec` OMap.empty) <$> arbName
            ]
              | otherwise = do
            n' <- QC.choose (0, n `div` 3)
            QC.oneof
                [ App <$> arbName <*> QC.liftArbitrary (arb n')
                , Rec <$> arbName <*> QC.liftArbitrary (arb n')
                , Lst <$> QC.liftArbitrary (arb n')
                ]

    shrink (Lst es)   = es
        ++ [ Lst es'    | es' <- QC.shrink es ]
    shrink (Rec n fs) = OMap.elems fs
        ++ [ Rec n' fs  | n'  <- QC.shrink n  ]
        ++ [ Rec n  fs' | fs' <- QC.shrink fs ]
    shrink (App n es) = es
        ++ [ App n' es  | n'  <- QC.shrink n  ]
        ++ [ App n  es' | es' <- QC.shrink es ]

arbName :: QC.Gen String
arbName = QC.frequency
    [ (10, QC.liftArbitrary $ QC.elements $ ['a'..'z'] ++ ['0' .. '9'] ++ "+-_:")
    , (1, show <$> (QC.arbitrary :: QC.Gen String))
    , (1, QC.arbitrary)
    , (1, QC.elements ["_×_", "_×_×_", "_×_×_×_"])
    ]

-- | Diff two 'Expr'.
--
-- For examples see 'ediff' in "Data.TreeDiff.Class".
exprDiff :: Expr -> Expr -> Edit EditExpr
exprDiff = impl
  where
    impl ea eb | ea == eb = Cpy (EditExp ea)

    -- application
    impl ea@(App a as) eb@(App b bs)
        | a == b    = Cpy $ EditApp a (map recurse (diffBy (==) as bs))
        | otherwise = Swp (EditExp ea) (EditExp eb)

    -- records
    impl ea@(Rec a as) eb@(Rec b bs)
        | a == b    = Cpy $ EditRec a $ alignWith cls as bs
        | otherwise = Swp (EditExp ea) (EditExp eb)
      where
        cls :: These Expr Expr -> Edit EditExpr
        cls (This x) = Del (EditExp x)
        cls (That y) = Ins (EditExp y)
        cls (These x y) = exprDiff x y

    -- lists
    impl (Lst as) (Lst bs) =
        Cpy $ EditLst (map recurse (diffBy (==) as bs))

    -- If higher level doesn't match, just swap.
    impl a b = Swp (EditExp a) (EditExp b)

    recurse (Ins x)   = Ins (EditExp x)
    recurse (Del y)   = Del (EditExp y)
    recurse (Cpy z)   = Cpy (EditExp z)
    recurse (Swp x y) = impl x y

-- | Type used in the result of 'ediff'.
data EditExpr
    = EditApp ConstructorName [Edit EditExpr]
    | EditRec ConstructorName (OMap FieldName (Edit EditExpr))
    | EditLst [Edit EditExpr]
    | EditExp Expr  -- ^ unchanged tree
  deriving Show

instance NFData EditExpr where
    rnf (EditApp n es) = rnf n `seq` rnf es
    rnf (EditRec n fs) = rnf n `seq` rnf fs
    rnf (EditLst es)   = rnf es
    rnf (EditExp e)    = rnf e
