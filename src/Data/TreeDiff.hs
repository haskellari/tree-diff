-- | Diffing of (expression) trees.
--
-- Diffing arbitrary Haskell data. First we convert values to untyped
-- haskell-like expression 'Expr' using generically derivable 'ToExpr' class.
-- Then we can diff two 'Expr' values.
-- The conversion and diffing is done by 'ediff' function.
-- See type and function haddocks for an examples.
--
-- Interesting modules:
--
-- * "Data.TreeDiff.Class" for a 'ToExpr' class and 'ediff' utility.
--
-- * "Data.TreeDiff.Golden" for golden tests helper
--
-- * "Data.TreeDiff.QuickCheck" for QuickCheck helper
--
module Data.TreeDiff (
    module Data.TreeDiff.Expr,
    module Data.TreeDiff.Class,
    module Data.TreeDiff.Pretty,
    module Data.TreeDiff.Parser,
    ) where

import Data.TreeDiff.Expr
import Data.TreeDiff.Class
import Data.TreeDiff.Pretty
import Data.TreeDiff.Parser
