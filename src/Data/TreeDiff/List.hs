{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A list diff.
module Data.TreeDiff.List (
    diffBy,
    Edit (..),
) where

import Control.DeepSeq (NFData (..))
import Control.Monad.ST (ST, runST)

import qualified Data.Primitive as P

-- import Debug.Trace

-- | List edit operations
--
-- The 'Swp' constructor is redundant, but it let us spot
-- a recursion point when performing tree diffs.
data Edit a
    = Ins a    -- ^ insert
    | Del a    -- ^ delete
    | Cpy a    -- ^ copy unchanged
    | Swp a a  -- ^ swap, i.e. delete + insert
  deriving (Eq, Show)

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
diffBy :: forall a. Show a => (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
diffBy _  [] []   = []
diffBy _  []  ys' = map Ins ys'
diffBy _  xs' []  = map Del xs'
diffBy eq xs' ys'
    | otherwise = reverse (getCell lcs)
  where
    xn = length xs'
    yn = length ys'

    xs = P.arrayFromListN xn xs'
    ys = P.arrayFromListN yn ys'

    lcs :: Cell [Edit a]
    lcs = runST $ do
        -- traceShowM ("sizes", xn, yn)

        -- create two buffers.
        buf1 <- P.newArray yn (Cell 0 [])
        buf2 <- P.newArray yn (Cell 0 [])

        -- fill the first row
        -- 0,0 case is filled already
        yLoop (Cell 0 []) $ \m (Cell w edit) -> do
            let cell = Cell (w + 1) (Ins (P.indexArray ys m) : edit)
            P.writeArray buf1 m cell
            P.writeArray buf2 m cell
            -- traceShowM ("init", m, cell)
            return cell

        -- following rows
        --
        -- cellC cellT
        -- cellL cellX
        (buf1final, _, _) <- xLoop (buf1, buf2, Cell 0 []) $ \n (prev, curr, cellC) -> do
            -- prevZ <- P.unsafeFreezeArray prev
            -- currZ <- P.unsafeFreezeArray prev
            -- traceShowM ("prev", n, prevZ)
            -- traceShowM ("curr", n, currZ)

            let cellL :: Cell [Edit a]
                cellL = case cellC of (Cell w edit) -> Cell (w + 1) (Del (P.indexArray xs n) : edit)

            -- traceShowM ("cellC, cellL", n, cellC, cellL)

            yLoop (cellC, cellL) $ \m (cellC', cellL') -> do
                -- traceShowM ("inner loop", n, m)
                cellT <- P.readArray prev m

                -- traceShowM ("cellT", n, m, cellT)

                let x, y :: a
                    x = P.indexArray xs n
                    y = P.indexArray ys m

                -- from diagonal
                let cellX1 :: Cell [Edit a]
                    cellX1
                        | eq x y    = bimap id   (Cpy x :)   cellC'
                        | otherwise = bimap (+1) (Swp x y :) cellC'

                -- from left
                let cellX2 :: Cell [Edit a]
                    cellX2 = bimap (+1) (Ins y :) cellL'

                -- from top
                let cellX3 :: Cell [Edit a]
                    cellX3 = bimap (+1) (Del x :) cellT

                -- the actual cell is best of three
                let cellX :: Cell [Edit a]
                    cellX = bestOfThree cellX1 cellX2 cellX3

                -- traceShowM ("cellX", n, m, cellX)

                -- memoize
                P.writeArray curr m cellX

                return (cellT, cellX)

            return (curr, prev, cellL)

        P.readArray buf1final (yn - 1)

    xLoop :: acc -> (Int -> acc -> ST s acc) -> ST s acc
    xLoop !acc0 f = go acc0 0 where
        go !acc !n | n < xn = do
            acc' <- f n acc
            go acc' (n + 1)
        go !acc _ = return acc

    yLoop :: acc -> (Int -> acc -> ST s acc) -> ST s ()
    yLoop !acc0 f = go acc0 0 where
        go !acc !m | m < yn = do
            acc' <- f m acc
            go acc' (m + 1)
        go _ _ = return ()

data Cell a = Cell !Int !a deriving Show

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
