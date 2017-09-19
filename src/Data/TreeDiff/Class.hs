{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TreeDiff.Class (
    ediff,
    ToExpr (..),
    defaultExprViaShow,
    -- * SOP
    sopToExpr,
    sopNPToExpr,
    ) where

import Data.Foldable      (toList)
import Data.Proxy         (Proxy (..))
import Data.TreeDiff.Expr
import Generics.SOP
       (All, All2, ConstructorInfo (..), DatatypeInfo (..), FieldInfo (..),
       I (..), K (..), NP (..), SOP (..), constructorInfo, hcliftA2, hcmap,
       hcollapse, mapIK)
import Generics.SOP.GGP   (GCode, GDatatypeInfo, GFrom, gdatatypeInfo, gfrom)
import GHC.Generics       (Generic)

import qualified Data.Map as Map

-- Instances
import Control.Applicative   (Const (..), ZipList (..))
import Data.Fixed            (Fixed, HasResolution)
import Data.Functor.Identity (Identity (..))
import Data.Int
import Data.List.NonEmpty    (NonEmpty (..))
import Data.Void             (Void)
import Data.Word
import Numeric.Natural       (Natural)

import qualified Data.Monoid    as Mon
import qualified Data.Ratio     as Ratio
import qualified Data.Semigroup as Semi

-- containers
import qualified Data.IntMap   as IntMap
import qualified Data.IntSet   as IntSet
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set
import qualified Data.Tree     as Tree

-- text
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

-- time
import qualified Data.Time as Time

-- bytestring
import qualified Data.ByteString.Char8      as LBS8
import qualified Data.ByteString.Lazy.Char8 as BS8

-- scientific
import Data.Scientific as Sci

-- uuid-types
import Data.UUID.Types as UUID

-- vector
import qualified Data.Vector           as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable  as VS
import qualified Data.Vector.Unboxed   as VU

-- tagged
import Data.Tagged (Tagged (..))

-- hashable
import Data.Hashable (Hashed, unhashed)

-- unordered-containers
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS

-- aeson
import qualified Data.Aeson as Aeson

-- | Difference between two 'ToExpr' values.
--
-- >>> let x = (1, Just 2) :: (Int, Maybe Int)
-- >>> let y = (1, Nothing)
-- >>> prettyEditExpr (ediff x y)
-- _,_ 1 -(Just 2) +Nothing
--
-- >>> data Foo = Foo { fooInt :: Either Char Int, fooBool :: [Maybe Bool], fooString :: String } deriving (Eq, Generic)
-- >>> instance ToExpr Foo
--
-- >>> prettyEditExpr $ ediff (Foo (Right 2) [Just True] "fo") (Foo (Right 3) [Just True] "fo")
-- Foo {fooBool = [Just True], fooInt = Right -2 +3, fooString = "fo"}
--
-- >>> prettyEditExpr $ ediff (Foo (Right 42) [Just True, Just False] "old") (Foo (Right 42) [Nothing, Just False, Just True] "new")
-- Foo
--   {fooBool = [-Just True, +Nothing, Just False, +Just True],
--    fooInt = Right 42,
--    fooString = -"old" +"new"}
--
ediff :: (ToExpr a, Eq a) => a -> a -> Edit EditExpr
ediff x y = exprDiff (toExpr x) (toExpr y)

-- |
--
-- >>> toExpr ((1, Just 2) :: (Int, Maybe Int))
-- App "_,_" [App "1" [],App "Just" [App "2" []]]
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

-- | >>> prettyExpr $ sopToExpr (gdatatypeInfo (Proxy :: Proxy String)) (gfrom "foo")
-- _:_ 'f' "oo"
sopToExpr :: (All2 ToExpr xss) => DatatypeInfo xss -> SOP I xss -> Expr
sopToExpr di (SOP xss) = hcollapse $ hcliftA2
    (Proxy :: Proxy (All ToExpr))
    (\ci xs -> K (sopNPToExpr isNewtype ci xs))
    (constructorInfo di)
    xss
  where
    isNewtype = case di of
        Newtype _ _ _ -> True
        ADT _ _ _     -> False

sopNPToExpr :: All ToExpr xs => Bool -> ConstructorInfo xs -> NP I xs -> Expr
sopNPToExpr _ (Infix cn _ _) xs = App ("_" ++ cn ++ "_") $ hcollapse $
    hcmap (Proxy :: Proxy ToExpr) (mapIK toExpr) xs
sopNPToExpr _ (Constructor cn) xs = App cn $ hcollapse $
    hcmap (Proxy :: Proxy ToExpr) (mapIK toExpr) xs
sopNPToExpr True (Record cn _) xs = App cn $ hcollapse $
    hcmap (Proxy :: Proxy ToExpr) (mapIK toExpr) xs
sopNPToExpr False (Record cn fi) xs = Rec cn $ Map.fromList $ hcollapse $
    hcliftA2 (Proxy :: Proxy ToExpr) mk fi xs
  where
    mk :: ToExpr x => FieldInfo x -> I x -> K (FieldName, Expr) x
    mk (FieldInfo fn) (I x) = K (fn, toExpr x)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance ToExpr () where toExpr = defaultExprViaShow
instance ToExpr Bool where toExpr = defaultExprViaShow
instance ToExpr Ordering where toExpr = defaultExprViaShow

instance ToExpr Integer where toExpr = defaultExprViaShow
instance ToExpr Natural where toExpr = defaultExprViaShow

instance ToExpr Float where toExpr = defaultExprViaShow
instance ToExpr Double where toExpr = defaultExprViaShow

instance ToExpr Int where toExpr = defaultExprViaShow
instance ToExpr Int8 where toExpr = defaultExprViaShow
instance ToExpr Int16 where toExpr = defaultExprViaShow
instance ToExpr Int32 where toExpr = defaultExprViaShow
instance ToExpr Int64 where toExpr = defaultExprViaShow

instance ToExpr Word where toExpr = defaultExprViaShow
instance ToExpr Word8 where toExpr = defaultExprViaShow
instance ToExpr Word16 where toExpr = defaultExprViaShow
instance ToExpr Word32 where toExpr = defaultExprViaShow
instance ToExpr Word64 where toExpr = defaultExprViaShow

instance ToExpr (Proxy a) where toExpr = defaultExprViaShow

-- | >>> prettyExpr $ toExpr 'a'
-- 'a'
--
-- >>> prettyExpr $ toExpr "Hello world"
-- "Hello world"
--
instance ToExpr Char where
    toExpr = defaultExprViaShow
    listToExpr = stringToExpr "unlines" lines

stringToExpr :: Show a => String -> (a -> [a]) -> a -> Expr
stringToExpr unlines_ lines_ s = case lines_ s of
    []  -> App (show s) []
    [l] -> defaultExprViaShow l
    ls  -> App unlines_ [Lst (map defaultExprViaShow ls)]

instance ToExpr a => ToExpr (Maybe a) where
    toExpr Nothing  = App "Nothing" []
    toExpr (Just x) = App "Just" [toExpr x]

instance (ToExpr a, ToExpr b) => ToExpr (Either a b) where
    toExpr (Left x)  = App "Left"  [toExpr x]
    toExpr (Right y) = App "Right" [toExpr y]

instance ToExpr a => ToExpr [a] where
    toExpr = listToExpr

instance (ToExpr a, ToExpr b) => ToExpr (a, b) where
    toExpr (a, b) = App "_,_" [toExpr a, toExpr b]
instance (ToExpr a, ToExpr b, ToExpr c) => ToExpr (a, b, c) where
    toExpr (a, b, c) = App "_,_,_" [toExpr a, toExpr b, toExpr c]
instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d) => ToExpr (a, b, c, d) where
    toExpr (a, b, c, d) = App "_,_,_,_" [toExpr a, toExpr b, toExpr c, toExpr d]
instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d, ToExpr e) => ToExpr (a, b, c, d, e) where
    toExpr (a, b, c, d, e) = App "_,_,_,_,_" [toExpr a, toExpr b, toExpr c, toExpr d, toExpr e]

-- | >>> prettyExpr $ toExpr (3 % 12 :: Rational)
-- _%_ 1 4
instance (ToExpr a, Integral a) => ToExpr (Ratio.Ratio a) where
    toExpr r = App "_%_" [ toExpr $ Ratio.numerator r, toExpr $ Ratio.denominator r ]
instance HasResolution a => ToExpr (Fixed a) where toExpr = defaultExprViaShow

-- | >>> prettyExpr $ toExpr $ Identity 'a'
-- Identity 'a'
instance ToExpr a => ToExpr (Identity a) where
    toExpr (Identity x) = App "Identity" [toExpr x]

instance ToExpr a => ToExpr (Const a b)
instance ToExpr a => ToExpr (ZipList a)

instance ToExpr a => ToExpr (NonEmpty a) where
    toExpr (x :| xs) = App "NE.fromList" [toExpr (x : xs)]

instance ToExpr Void where
    toExpr _ = App "error" [toExpr "Void"]

-------------------------------------------------------------------------------
-- Monoid/semigroups
-------------------------------------------------------------------------------

instance ToExpr a => ToExpr (Mon.Dual a) where
instance ToExpr a => ToExpr (Mon.Sum a) where
instance ToExpr a => ToExpr (Mon.Product a) where
instance ToExpr a => ToExpr (Mon.First a) where
instance ToExpr a => ToExpr (Mon.Last a) where

instance ToExpr a => ToExpr (Semi.Option a) where
instance ToExpr a => ToExpr (Semi.Min a) where
instance ToExpr a => ToExpr (Semi.Max a) where
instance ToExpr a => ToExpr (Semi.First a) where
instance ToExpr a => ToExpr (Semi.Last a) where

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance ToExpr a => ToExpr (Tree.Tree a) where
    toExpr (Tree.Node x xs) = App "Node" [toExpr x, toExpr xs]

instance (ToExpr k, ToExpr v) => ToExpr (Map.Map k v) where
    toExpr x = App "Map.fromList" [ toExpr $ Map.toList x ]
instance (ToExpr k) => ToExpr (Set.Set k) where
    toExpr x = App "Set.fromList" [ toExpr $ Set.toList x ]
instance (ToExpr v) => ToExpr (IntMap.IntMap v) where
    toExpr x = App "IntMap.fromList" [ toExpr $ IntMap.toList x ]
instance ToExpr IntSet.IntSet where
    toExpr x = App "IntSet.fromList" [ toExpr $ IntSet.toList x ]
instance (ToExpr v) => ToExpr (Seq.Seq v) where
    toExpr x = App "Seq.fromList" [ toExpr $ toList x ]

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

instance ToExpr LT.Text where
    toExpr = stringToExpr "LT.unlines" LT.lines

instance ToExpr T.Text where
    toExpr = stringToExpr "T.unlines" T.lines

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

-- | >>> prettyExpr $ toExpr $ ModifiedJulianDay 58014
-- Day "2017-09-18"
instance ToExpr Time.Day where
    toExpr d = App "Day" [ toExpr (show d) ]

instance ToExpr Time.UTCTime where
    toExpr t = App "UTCTime" [ toExpr (show t) ]

-------------------------------------------------------------------------------
-- bytestring
-------------------------------------------------------------------------------

instance ToExpr LBS8.ByteString where
    toExpr = stringToExpr "LBS8.unlines" LBS8.lines

instance ToExpr BS8.ByteString where
    toExpr = stringToExpr "BS8.unlines" BS8.lines

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

-- | >>> prettyExpr $ toExpr (123.456 :: Scientific)
-- scientific 123456 `-3`
instance ToExpr Sci.Scientific where
    toExpr s = App "scientific" [ toExpr $ Sci.coefficient s, toExpr $ Sci.base10Exponent s ]

-------------------------------------------------------------------------------
-- uuid-types
-------------------------------------------------------------------------------

-- | >>> prettyExpr $ toExpr UUID.nil
-- UUID "00000000-0000-0000-0000-000000000000"
instance ToExpr UUID.UUID where
    toExpr u = App "UUID" [ toExpr $ toString u ]

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance ToExpr a => ToExpr (V.Vector a) where
    toExpr x = App "V.fromList" [ toExpr $ V.toList x ]
instance (ToExpr a, VU.Unbox a) => ToExpr (VU.Vector a) where
    toExpr x = App "VU.fromList" [ toExpr $ VU.toList x ]
instance (ToExpr a, VS.Storable a) => ToExpr (VS.Vector a) where
    toExpr x = App "VS.fromList" [ toExpr $ VS.toList x ]
instance (ToExpr a, VP.Prim a) => ToExpr (VP.Vector a) where
    toExpr x = App "VP.fromList" [ toExpr $ VP.toList x ]

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance ToExpr a => ToExpr (Tagged t a) where
    toExpr (Tagged x) = App "Tagged" [ toExpr x ]

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

instance ToExpr a => ToExpr (Hashed a) where
    toExpr x = App "hashed" [ toExpr $ unhashed x ]

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance (ToExpr k, ToExpr v) => ToExpr (HM.HashMap k v) where
    toExpr x = App "HM.fromList" [ toExpr $ HM.toList x ]
instance (ToExpr k) => ToExpr (HS.HashSet k) where
    toExpr x = App "HS.fromList" [ toExpr $ HS.toList x ]

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance ToExpr Aeson.Value

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> import Data.Ratio ((%))
-- >>> import Data.Time (Day (..))
-- >>> import Data.Scientific (Scientific)
-- >>> import Data.TreeDiff.Pretty
