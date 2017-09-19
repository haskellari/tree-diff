module Main where

import Data.TreeDiff
import Prelude ()
import Prelude.Compat
import System.Console.ANSI   (SGR (Reset), setSGRCode)
import Test.QuickCheck       (Property, counterexample)
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Trifecta         (eof, parseString)
import Text.Trifecta.Result  (ErrInfo (..), Result (..))


main :: IO ()
main = defaultMain $ testGroup "tests"
    [ testProperty "trifecta-pretty roundtrip" roundtripPretty
    ]

roundtripPretty :: Expr -> Property
roundtripPretty e = counterexample info $ ediffEq (Just e) res'
  where
    doc = show (prettyExpr e)
    res = parseString (exprParser <* eof)  mempty doc

    info = case res of
        Success e'  ->
            doc
            ++ "\n" ++
            show e'
        Failure err ->
            doc
            ++ "\n" ++
            show (_errDoc err)

    res' = case res of
        Success e' -> Just e'
        Failure _  -> Nothing

ediffEq :: (Eq a, ToExpr a) => a -> a -> Property
ediffEq x y = counterexample
    (setSGRCode [Reset] ++ show (ansiWlEditExpr $ ediff x y))
    (x == y)
