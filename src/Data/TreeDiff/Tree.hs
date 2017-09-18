{-# LANGUAGE CPP #-}
-- | Tree diffing working on @containers@ 'Tree'.
module Data.TreeDiff.Tree (treeDiff, EditTree (..), Edit (..)) where

import Data.Tree (Tree (..))
import Data.TreeDiff.List

#ifdef __DOCTEST__
import qualified Text.PrettyPrint as PP
#endif

-- | A breadth-traversal diff.
--
-- It's different from @gdiff@, as it doesn't produce a flat edit script,
-- but edit script iself is a tree. This makes visualising the diff much
-- simpler.
--
-- ==== Examples
--
-- Let's start from simple tree. We pretty print them as s-expressions.
--
-- >>> let x = Node 'a' [Node 'b' [], Node 'c' [return 'd', return 'e'], Node 'f' []]
-- >>> ppTree PP.char x
-- (a b (c d e) f)
--
-- If we modify an argument in a tree, we'll notice it's changed:
--
-- >>> let y = Node 'a' [Node 'b' [], Node 'c' [return 'x', return 'e'], Node 'f' []]
-- >>> ppTree PP.char y
-- (a b (c x e) f)
--
-- >>> ppEditTree PP.char (treeDiff x y)
-- (a b (c -d +x e) f)
--
-- If we modify a constructor, the whole sub-trees is replaced, though there
-- might be common subtrees.
--
-- >>> let z = Node 'a' [Node 'b' [], Node 'd' [], Node 'f' []]
-- >>> ppTree PP.char z
-- (a b d f)
--
-- >>> ppEditTree PP.char (treeDiff x z)
-- (a b -(c d e) +d f)
--
-- If we add arguments, they are spotted too:
--
-- >>> let w = Node 'a' [Node 'b' [], Node 'c' [return 'd', return 'x', return 'e'], Node 'f' []]
-- >>> ppTree PP.char w
-- (a b (c d x e) f)
--
-- >>> ppEditTree PP.char (treeDiff x w)
-- (a b (c d +x e) f)
--
treeDiff :: Eq a => Tree a -> Tree a -> Edit (EditTree a)
treeDiff ta@(Node a as) tb@(Node b bs)
    | a == b = Cpy $ EditNode a (map rec (diffBy (==) as bs))
    | otherwise = Swp (treeToEdit ta) (treeToEdit tb)
  where
    rec (Ins x)   = Ins (treeToEdit x)
    rec (Del y)   = Del (treeToEdit y)
    rec (Cpy z)   = Cpy (treeToEdit z)
    rec (Swp x y) = treeDiff x y

data EditTree a
    = EditNode a [Edit (EditTree a)]
  deriving Show

treeToEdit :: Tree a -> EditTree a
treeToEdit = go where go (Node x xs) = EditNode x (map (Cpy . go) xs)

#ifdef __DOCTEST__
ppTree :: (a -> PP.Doc) -> Tree a -> PP.Doc
ppTree pp = ppT
  where
    ppT (Node x []) = pp x
    ppT (Node x xs) = PP.parens $ PP.hang (pp x) 2 $
        PP.sep $ map ppT xs

ppEditTree :: (a -> PP.Doc) -> Edit (EditTree a) -> PP.Doc
ppEditTree pp = PP.sep . ppEdit
  where
    ppEdit (Cpy tree) = [ ppTree tree ]
    ppEdit (Ins tree) = [ PP.char '+' PP.<> ppTree tree ]
    ppEdit (Del tree) = [ PP.char '-' PP.<> ppTree tree ]
    ppEdit (Swp a b) =
        [ PP.char '-' PP.<> ppTree a
        , PP.char '+' PP.<> ppTree b
        ]

    ppTree (EditNode x []) = pp x
    ppTree (EditNode x xs) = PP.parens $ PP.hang (pp x) 2 $
       PP.sep $ concatMap ppEdit xs
#endif
