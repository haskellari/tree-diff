-- | @QuickCheck@ related utilities.
module Data.TreeDiff.QuickCheck (
    ediffEq,
    ) where

import Data.TreeDiff
import System.Console.ANSI (SGR (Reset), setSGRCode)
import Test.QuickCheck     (Property, counterexample)

-- | A variant of '===', which outputs a diff when values are inequal.
ediffEq :: (Eq a, ToExpr a) => a -> a -> Property
ediffEq x y = counterexample
    (setSGRCode [Reset] ++ show (ansiEditExpr $ ediff x y))
    (x == y)
