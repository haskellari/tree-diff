-- | @QuickCheck@ related utilities.
module Data.TreeDiff.QuickCheck (
    ediffEq,
    ) where

import Data.TreeDiff
import System.Console.ANSI (SGR (Reset), setSGRCode)
import Test.QuickCheck     (Property, counterexample)

import qualified Data.Text.Lazy                as TL
import qualified Prettyprinter                 as PP
import qualified Prettyprinter.Render.Terminal as PP.A

-- | A variant of '===', which outputs a diff when values are inequal.
ediffEq :: (Eq a, ToExpr a) => a -> a -> Property
ediffEq x y = counterexample
    (setSGRCode [Reset] ++ render (ansiWlEditExpr $ ediff x y))
    (x == y)

render :: PP.Doc PP.A.AnsiStyle -> String
render = TL.unpack . PP.A.renderLazy . PP.layoutPretty PP.defaultLayoutOptions
