{-# LANGUAGE DeriveFunctor #-}
-- | Map which remembers the 'fromList' order.
-- This module is minimal on purpose.
module Data.TreeDiff.OMap (
    -- * Ordered map
    OMap,
    -- * Conversions
    toAscList,
    toList,
    fromList,
    -- * Construction
    empty,
    -- * Query
    elems,
) where

import Control.DeepSeq (NFData (..))
import Data.List       (sortBy)
import Data.Ord        (comparing)
import Data.Semialign  (Semialign (..))
import Data.These      (These (..))

import qualified Data.Map.Strict as Map
import qualified Test.QuickCheck as QC

-- $setup
-- >>> import Data.Semialign (alignWith)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype OMap k v = OMap (Map.Map k (Val v))
  deriving (Functor)

-- Value with its index
data Val v = Val !Int v
  deriving (Functor)

-- | Note: The instance uses 'toList', so 'Eq'ual 'OMap's can be shown differently.
instance (Show k, Show v) => Show (OMap k v) where
    showsPrec d m = showParen (d > 10)
        $ showString "fromList "
        . showsPrec d (toList m)

-- |
--
-- >>> xs = toAscList $ fromList [('a', "alpha"), ('b', "beta"), ('g', "gamma")]
-- >>> ys = toAscList $ fromList [('g', "gamma"), ('b', "beta"), ('a', "alpha")]
-- >>> xs == ys
-- True
--
-- >>> zs = toAscList $ fromList [('d', "delta"), ('b', "beta"), ('a', "alpha")]
-- >>> xs == zs
-- False
--
instance (Eq k, Eq v) => Eq (OMap k v) where
    xs == ys = toAscList xs == toAscList ys

instance (Ord k, Ord v) => Ord (OMap k v) where
    compare xs ys = compare (toAscList xs) (toAscList ys)

-------------------------------------------------------------------------------
-- deepseq
-------------------------------------------------------------------------------

instance NFData v => NFData (Val v) where
    rnf (Val _ v) = rnf v

instance (NFData k, NFData v) => NFData (OMap k v) where
    rnf (OMap m) = rnf m

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

instance (Ord k, QC.Arbitrary k, QC.Arbitrary v) => QC.Arbitrary (OMap k v) where
    arbitrary = QC.arbitrary1
    shrink    = QC.shrink1

instance (Ord k, QC.Arbitrary k) => QC.Arbitrary1 (OMap k) where
    liftArbitrary arb = fmap fromList $ QC.liftArbitrary (QC.liftArbitrary arb)
    liftShrink shr m  = fmap fromList $ QC.liftShrink (QC.liftShrink shr) $ toList m

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

-- |
--
-- >>> empty :: OMap String Integer
-- fromList []
--
empty :: OMap k v
empty = OMap Map.empty

-- | Elements in key ascending order.
elems :: OMap k v -> [v]
elems (OMap m) = map (snd . getVal) $ Map.toAscList m

-- |
--
-- >>> toAscList $ fromList [('a', "alpha"), ('b', "beta"), ('g', "gamma")]
-- [('a',"alpha"),('b',"beta"),('g',"gamma")]
--
-- >>> toAscList $ fromList [('g', "gamma"), ('b', "beta"), ('a', "alpha")]
-- [('a',"alpha"),('b',"beta"),('g',"gamma")]
--
toAscList :: OMap k v -> [(k, v)]
toAscList (OMap m) = map getVal $ Map.toAscList m

-- | /O(n log n)/. List in creation order.
-- Doesn't respect 'Eq' instance.
--
-- >>> toList $ fromList [('a', "alpha"), ('b', "beta"), ('g', "gamma")]
-- [('a',"alpha"),('b',"beta"),('g',"gamma")]
--
-- >>> toList $ fromList [('g', "gamma"), ('b', "beta"), ('a', "alpha")]
-- [('g',"gamma"),('b',"beta"),('a',"alpha")]
--
toList :: OMap k v -> [(k, v)]
toList (OMap m) = map getVal $ sortBy (comparing getIdx) $ Map.toList m

getIdx :: (k, Val v) -> Int
getIdx (_, Val i _) = i

getVal :: (k, Val v) -> (k, v)
getVal (k, Val _ v) = (k, v)

-- |
--
-- >>> fromList [('g', "gamma"), ('b', "beta"), ('a', "alpha")]
-- fromList [('g',"gamma"),('b',"beta"),('a',"alpha")]
--
fromList :: Ord k => [(k, v)] -> OMap k v
fromList kvs = OMap (Map.fromList (zipWith p [0..] kvs)) where
    p i (k, v) = (k, Val i v)

-- |
--
-- >>> let xs = fromList [('a', "alpha"), ('b', "beta")]
-- >>> let ys = fromList [('c', 3 :: Int), ('b', 2)]
-- >>> alignWith id xs ys
-- fromList [('a',This "alpha"),('c',That 3),('b',These "beta" 2)]
--
instance Ord k => Semialign (OMap k) where
    alignWith f (OMap xs) (OMap ys) = OMap (alignWith g xs ys) where
        g (This (Val i x))            = Val i (f (This x))
        g (That (Val j y))            = Val j (f (That y))
        g (These (Val i x) (Val j y)) = Val (min i j) (f (These x y))
