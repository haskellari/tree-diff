{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
-- | A list diff.
module Data.TreeDiff.List (diffBy, Edit (..)) where

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

-- | List edit operations
--
-- The 'Swp' constructor is redundant, but it let us spot
-- a recursion point when performing tree diffs.
data Edit a
    = Ins a    -- ^ insert
    | Del a    -- ^ delete
    | Cpy a    -- ^ copy unchanged
    | Swp a a  -- ^ swap, i.e. delete + insert
  deriving (Eq, Generic, Generic1, Show)

instance NFData a => NFData (Edit a)
instance NFData1 Edit

type WEdit a = (Int, [Edit a])
type WEdits a = [WEdit a]

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
diffBy eq xs' ys' = reverse (snd (last (foldl (nextRow ys') row0 xs')))
  where
    row0 :: WEdits a
    row0 = scanl (\(w, is) i -> (w+1, Ins i: is)) (0, []) ys'
    nextCell :: a -> WEdit a -> a -> WEdit a -> WEdit a -> WEdit a
    nextCell x (l, le) y (lt, lte) (t, te)
      | eq x y = (lt, Cpy x : lte)
      | lt <= t && lt <= l = (lt+1, Swp x y:lte)
      | l <= t = (l+1, Ins y:le)
      | otherwise = (t+1, Del x:te)
    curryNextCell :: a -> WEdit a -> ((a, WEdit a), WEdit a) -> WEdit a
    curryNextCell x l = uncurry (uncurry (nextCell x l))
    nextRow :: [a] -> WEdits a -> a -> WEdits a
    nextRow ys da@(~((dn, de):ds)) x = scanl (curryNextCell x) (dn+1,Del x:de) (zip (zip ys da) ds)
