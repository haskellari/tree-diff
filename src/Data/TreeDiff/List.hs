{-# LANGUAGE ScopedTypeVariables #-}
-- | A list diff.
module Data.TreeDiff.List (diffBy, Edit (..)) where

import Control.DeepSeq (NFData (..))

import qualified Data.Primitive as P

-- | List edit operations
--
-- The 'Swp' constructor is redundant, but it let us spot
-- a recursion point when performing tree diffs.
data Edit a
    = Ins a    -- ^ insert
    | Del a    -- ^ delete
    | Cpy a    -- ^ copy unchanged
    | Swp a a  -- ^ swap, i.e. delete + insert
  deriving Show

instance NFData a => NFData (Edit a) where
    rnf (Ins x)   = rnf x
    rnf (Del x)   = rnf x
    rnf (Cpy x)   = rnf x
    rnf (Swp x y) = rnf x `seq` rnf y

-- | List difference.
--
-- >>> diffBy (==) "hello" "world"
-- [Swp 'h' 'w',Swp 'e' 'o',Swp 'l' 'r',Cpy 'l',Swp 'o' 'd']
--
-- >>> diffBy (==) "kitten" "sitting"
-- [Swp 'k' 's',Cpy 'i',Cpy 't',Cpy 't',Swp 'e' 'i',Cpy 'n',Ins 'g']
--
-- prop> \xs ys -> length (diffBy (==) xs ys) >= max (length xs) (length (ys :: String))
-- prop> \xs ys -> length (diffBy (==) xs ys) <= length xs + length (ys :: String)
--
-- /Note:/ currently this has O(n*m) memory requirements, for the sake
-- of more obviously correct implementation.
--
diffBy :: forall a. (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
diffBy eq xs' ys' = reverse (getCell (lcs xn yn))
  where
    xn = length xs'
    yn = length ys'

    xs = P.arrayFromListN xn xs'
    ys = P.arrayFromListN yn ys'

    memo :: P.Array (Cell [Edit a])
    memo = P.arrayFromListN ((xn + 1) * (yn + 1))
        [ impl xi yi
        | xi <- [0 .. xn]
        , yi <- [0 .. yn]
        ]

    lcs :: Int -> Int -> Cell [Edit a]
    lcs xi yi = P.indexArray memo (yi + xi * (yn + 1))

    impl :: Int -> Int -> Cell [Edit a]
    impl 0 0 = Cell 0 []
    impl 0 m = case lcs 0 (m - 1) of
        Cell w edit -> Cell (w + 1) (Ins (P.indexArray ys (m - 1)) : edit)
    impl n 0 = case lcs (n - 1) 0 of
        Cell w edit -> Cell (w + 1) (Del (P.indexArray xs (n - 1)) : edit)

    impl n m = bestOfThree
        edit
        (bimap (+1) (Ins y :) (lcs n (m - 1)))
        (bimap (+1) (Del x :) (lcs (n - 1) m))
      where
        x = P.indexArray xs (n - 1)
        y = P.indexArray ys (m - 1)

        edit
            | eq x y    = bimap id   (Cpy x :)   (lcs (n - 1) (m - 1))
            | otherwise = bimap (+1) (Swp x y :) (lcs (n - 1) (m - 1))

data Cell a = Cell !Int !a

getCell :: Cell a -> a
getCell (Cell _ x) = x

bestOfThree :: Cell a -> Cell a -> Cell a -> Cell a
bestOfThree a@(Cell i _x) b@(Cell j _y) c@(Cell k _z)
    | i <= j
    = if i <= k then a else c

    | otherwise
    = if j <= k then b else c

bimap :: (Int -> Int) -> (a -> b) -> Cell a -> Cell b
bimap f g (Cell i x) = Cell (f i) (g x)
