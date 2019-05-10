{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A 'ToExpr' class.
module Data.TreeDiff.Class (
    ediff,
    ediff',
    ToExpr (..),
    defaultExprViaShow,
    -- * SOP
    sopToExpr,
    ) where

import Data.Foldable      (toList)
import Data.Proxy         (Proxy (..))
import Data.TreeDiff.Expr
import Data.List.Compat      (uncons)
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
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- scientific
import qualified Data.Scientific as Sci

-- uuid-types
import qualified Data.UUID.Types as UUID

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
-- _×_ 1 -(Just 2) +Nothing
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
ediff :: ToExpr a => a -> a -> Edit EditExpr
ediff x y = exprDiff (toExpr x) (toExpr y)

-- | Compare different types.
--
-- /Note:/ Use with care as you can end up comparing apples with oranges.
--
-- >>> prettyEditExpr $ ediff' ["foo", "bar"] [Just "foo", Nothing]
-- [-"foo", +Just "foo", -"bar", +Nothing]
--
ediff' :: (ToExpr a, ToExpr b) => a -> b -> Edit EditExpr
ediff' x y = exprDiff (toExpr x) (toExpr y)

-- | 'toExpr' converts a Haskell value into
-- untyped Haskell-like syntax tree, 'Expr'.
--
-- >>> toExpr ((1, Just 2) :: (Int, Maybe Int))
-- App "_\215_" [App "1" [],App "Just" [App "2" []]]
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

-- | An alternative implementation for literal types. We use 'show'
-- representation of them.
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
        Newtype {} -> True
        ADT {}     -> False

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
-- >>> prettyExpr $ toExpr "Hello\nworld"
-- concat ["Hello\n", "world"]
--
-- >>> traverse_ (print . prettyExpr . toExpr) ["", "\n", "foo", "foo\n", "foo\nbar", "foo\nbar\n"]
-- ""
-- "\n"
-- "foo"
-- "foo\n"
-- concat ["foo\n", "bar"]
-- concat ["foo\n", "bar\n"]
--
instance ToExpr Char where
    toExpr = defaultExprViaShow
    listToExpr = stringToExpr "concat" . unconcat uncons

stringToExpr
    :: Show a
    => String -- ^ name of concat
    -> [a]
    -> Expr
stringToExpr _  []  = App "\"\"" []
stringToExpr _  [l] = defaultExprViaShow l
stringToExpr cn ls  = App cn [Lst (map defaultExprViaShow ls)]

-- | Split on '\n'.
--
-- prop> \xs -> xs == concat (unconcat uncons xs)
unconcat :: forall a. (a -> Maybe (Char, a)) -> a -> [String]
unconcat uncons_ = go where
    go :: a -> [String]
    go xs = case span_ xs of
        ~(ys, zs)
            | null ys   -> []
            | otherwise -> ys : go zs

    span_ :: a -> (String, a)
    span_ xs = case uncons_ xs of
        Nothing         -> ("", xs)
        Just ~(x, xs')
            | x == '\n' -> ("\n", xs')
            | otherwise -> case span_ xs' of
            ~(ys, zs) -> (x : ys, zs)

instance ToExpr a => ToExpr (Maybe a) where
    toExpr Nothing  = App "Nothing" []
    toExpr (Just x) = App "Just" [toExpr x]

instance (ToExpr a, ToExpr b) => ToExpr (Either a b) where
    toExpr (Left x)  = App "Left"  [toExpr x]
    toExpr (Right y) = App "Right" [toExpr y]

instance ToExpr a => ToExpr [a] where
    toExpr = listToExpr

instance (ToExpr a, ToExpr b) => ToExpr (a, b) where
    toExpr (a, b) = App "_×_" [toExpr a, toExpr b]
instance (ToExpr a, ToExpr b, ToExpr c) => ToExpr (a, b, c) where
    toExpr (a, b, c) = App "_×_×_" [toExpr a, toExpr b, toExpr c]
instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d) => ToExpr (a, b, c, d) where
    toExpr (a, b, c, d) = App "_×_×_×_" [toExpr a, toExpr b, toExpr c, toExpr d]
instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d, ToExpr e) => ToExpr (a, b, c, d, e) where
    toExpr (a, b, c, d, e) = App "_×_×_×_×_" [toExpr a, toExpr b, toExpr c, toExpr d, toExpr e]

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

-- | >>> traverse_ (print . prettyExpr . toExpr . LT.pack) ["", "\n", "foo", "foo\n", "foo\nbar", "foo\nbar\n"]
-- ""
-- "\n"
-- "foo"
-- "foo\n"
-- LT.concat ["foo\n", "bar"]
-- LT.concat ["foo\n", "bar\n"]
instance ToExpr LT.Text where
    toExpr = stringToExpr "LT.concat" . unconcat LT.uncons

-- | >>> traverse_ (print . prettyExpr . toExpr . T.pack) ["", "\n", "foo", "foo\n", "foo\nbar", "foo\nbar\n"]
-- ""
-- "\n"
-- "foo"
-- "foo\n"
-- T.concat ["foo\n", "bar"]
-- T.concat ["foo\n", "bar\n"]
instance ToExpr T.Text where
    toExpr = stringToExpr "T.concat" . unconcat T.uncons

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

-- | >>> traverse_ (print . prettyExpr . toExpr . LBS8.pack) ["", "\n", "foo", "foo\n", "foo\nbar", "foo\nbar\n"]
-- ""
-- "\n"
-- "foo"
-- "foo\n"
-- LBS.concat ["foo\n", "bar"]
-- LBS.concat ["foo\n", "bar\n"]
instance ToExpr LBS.ByteString where
    toExpr = stringToExpr "LBS.concat" . bsUnconcat LBS.null LBS.elemIndex LBS.splitAt

-- | >>> traverse_ (print . prettyExpr . toExpr . BS8.pack) ["", "\n", "foo", "foo\n", "foo\nbar", "foo\nbar\n"]
-- ""
-- "\n"
-- "foo"
-- "foo\n"
-- BS.concat ["foo\n", "bar"]
-- BS.concat ["foo\n", "bar\n"]
instance ToExpr BS.ByteString where
    toExpr = stringToExpr "BS.concat" . bsUnconcat BS.null BS.elemIndex BS.splitAt

-- | >>> traverse_ (print . prettyExpr . toExpr . SBS.toShort . BS8.pack) ["", "\n", "foo", "foo\n", "foo\nbar", "foo\nbar\n"]
-- ""
-- "\n"
-- "foo"
-- "foo\n"
-- mconcat ["foo\n", "bar"]
-- mconcat ["foo\n", "bar\n"]
instance ToExpr SBS.ShortByteString where
    toExpr = stringToExpr "mconcat" . bsUnconcat BS.null BS.elemIndex BS.splitAt . SBS.fromShort

bsUnconcat
    :: forall bs int. Num int
    => (bs -> Bool)
    -> (Word8 -> bs -> Maybe int)
    -> (int -> bs -> (bs, bs))
    -> bs
    -> [bs]
bsUnconcat null_ elemIndex_ splitAt_ = go where
    go :: bs -> [bs]
    go bs
        | null_ bs  = []
        | otherwise = case elemIndex_ 10 bs of
            Nothing -> [bs]
            Just i  -> case splitAt_ (i + 1) bs of
                (bs0, bs1) -> bs0 : go bs1

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
    toExpr u = App "UUID" [ toExpr $ UUID.toString u ]

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
-- >>> :set -XDeriveGeneric
-- >>> import Data.Foldable (traverse_)
-- >>> import Data.Ratio ((%))
-- >>> import Data.Time (Day (..))
-- >>> import Data.Scientific (Scientific)
-- >>> import Data.TreeDiff.Pretty
-- >>> import qualified Data.ByteString.Char8 as BS8
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
