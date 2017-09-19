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

import Data.Map           (Map)
import Data.TreeDiff.List

import qualified Data.Map        as Map
import qualified Test.QuickCheck as QC

type ConstructorName = String
type FieldName       = String

-- | An expression tree.
data Expr
    = App ConstructorName [Expr]
    | Rec ConstructorName (Map FieldName Expr)
    | Lst [Expr]
  deriving (Eq, Show)

instance QC.Arbitrary Expr where
    arbitrary = QC.scale (min 25) $ QC.sized arb where
        arb n | n <= 0 = QC.oneof
            [ (`App` []) <$> arbName
            ,  (`Rec` mempty) <$> arbName
            ]
        arb n | otherwise = do
            n' <- QC.choose (0, n `div` 3)
            QC.oneof
                [ App <$> arbName <*> QC.liftArbitrary (arb n')
                , Rec <$> arbName <*> QC.liftArbitrary (arb n')
                , Lst <$> QC.liftArbitrary (arb n')
                ]

    shrink (Lst es)   = es
        ++ [ Lst es'    | es' <- QC.shrink es ]
    shrink (Rec n fs) = Map.elems fs
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

    impl ea@(App a as) eb@(App b bs)
        | a == b = Cpy $ EditApp a (map recurse (diffBy (==) as bs))
        | otherwise = Swp (EditExp ea) (EditExp eb)
    impl ea@(Rec a as) eb@(Rec b bs)
        | a == b = Cpy $ EditRec a $ Map.unions [inter, onlyA, onlyB]
        | otherwise = Swp (EditExp ea) (EditExp eb)
      where
        inter = Map.intersectionWith exprDiff as bs
        onlyA = fmap (Del . EditExp) (Map.difference as inter)
        onlyB = fmap (Ins . EditExp) (Map.difference bs inter)
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
