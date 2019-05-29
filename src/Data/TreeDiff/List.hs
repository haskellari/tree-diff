{-# LANGUAGE ScopedTypeVariables #-}
-- | A list diff.
module Data.TreeDiff.List (diffBy, Edit (..)) where

import Data.List.Compat (sortOn)
import qualified Data.Vector as V

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
diffBy eq xs' ys' = reverse (snd (lcs xn yn))
  where
    xn = V.length xs
    yn = V.length ys

    xs = V.fromList xs'
    ys = V.fromList ys'

    memo :: V.Vector (Int, [Edit a])
    memo = V.fromList
        [ impl xi yi
        | xi <- [0 .. xn]
        , yi <- [0 .. yn]
        ]

    lcs :: Int -> Int -> (Int, [Edit a])
    lcs xi yi = memo V.! (yi + xi * (yn + 1))

    impl :: Int -> Int -> (Int, [Edit a])
    impl 0 0 = (0, [])
    impl 0 m = case lcs 0 (m-1) of
        (w, edit) -> (w + 1, Ins (ys V.! (m - 1)) : edit)
    impl n 0 = case lcs (n -1) 0 of
        (w, edit) -> (w + 1, Del (xs V.! (n - 1)) : edit)

    impl n m = head $ sortOn fst
        [ edit
        , bimap (+1) (Ins y :) (lcs n (m - 1))
        , bimap (+1) (Del x :) (lcs (n - 1) m)
        ]
      where
        x = xs V.! (n - 1)
        y = ys V.! (m - 1)

        edit
            | eq x y    = bimap id   (Cpy x :)   (lcs (n - 1) (m - 1))
            | otherwise = bimap (+1) (Swp x y :) (lcs (n -1 ) (m - 1))

bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimap f g (x, y) = (f x, g y)
