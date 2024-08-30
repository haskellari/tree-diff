-- | @QuickCheck@ related utilities.
module Data.TreeDiff.QuickCheck (
    ediffEq,
    ) where

import qualified Data.Text.Lazy                as TL
import           Data.TreeDiff
import           Prettyprinter
                 (defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Terminal (renderLazy)
import           System.Console.ANSI           (SGR (Reset), setSGRCode)
import           Test.QuickCheck               (Property, counterexample)

-- | A variant of '===', which outputs a diff when values are inequal.
ediffEq :: (Eq a, ToExpr a) => a -> a -> Property
ediffEq x y = counterexample
    (setSGRCode [Reset] ++ render (ansiWlEditExpr $ ediff x y))
    (x == y)
    where
        render = TL.unpack . renderLazy . layoutPretty defaultLayoutOptions
