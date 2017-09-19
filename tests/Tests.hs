{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Data.Proxy                 (Proxy (..))
import Data.TreeDiff
import Data.TreeDiff.Golden
import Data.TreeDiff.QuickCheck
import GHC.Generics               (Generic)
import Prelude ()
import Prelude.Compat
import Test.QuickCheck            (Property, counterexample)
import Test.Tasty                 (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.QuickCheck      (testProperty)
import Text.Trifecta              (eof, parseString)
import Text.Trifecta.Result       (ErrInfo (..), Result (..))

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ testProperty "trifecta-pretty roundtrip" roundtripPretty
    , exFooTests
    ]

-------------------------------------------------------------------------------
-- QuickCheck: ediffEq
-------------------------------------------------------------------------------

-- | This property tests that we can parse pretty printed 'Expr'.
--
-- We demonstrate the use of 'ediffEq'. We could used '===' there,
-- but now the nice diff will be printed as well
-- (as there is 'ToExpr Expr' instance).
roundtripPretty :: Expr -> Property
roundtripPretty e = counterexample info $ ediffEq (Just e) res'
  where
    doc = show (prettyExpr e)
    res = parseString (exprParser <* eof) mempty doc

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

-------------------------------------------------------------------------------
-- Golden
-------------------------------------------------------------------------------

-- | This test case verifies that we don't change 'Foo' or 'exFoo'.
--
-- We demonstrate the use of 'ediffGolden'.
--
-- First we declare a type, make it instance of 'ToExpr' and define
-- an example value 'exFoo'. In real-world you might e.g. read the source
-- file and parse it into the AST type.
--
-- Then we create a golden test that verifies that version we got now,
-- is the same we had previously. @tree-diff@ seralises the 'Expr',
-- not the original value. This is a design trade-off: 
-- as we can always deserialise we can better diff the values even the 
-- type is changed, e.g. the fields is added.
data Foo = Foo
    { fooInt :: Int
    , fooBar :: [Maybe String]
    , fooQuu :: (Double, Proxy ())
    -- , fooNew :: Bool
    }
  deriving (Eq, Show, Generic)

instance ToExpr Foo

exFoo :: Foo
exFoo = Foo
    { fooInt = 42
    , fooBar = [Just "pub", Just "night\nclub"]
    , fooQuu = (125.375, Proxy)
    -- , fooNew = True
    }

exFooTests :: TestTree
exFooTests = ediffGolden goldenTest "golden exFoo" "fixtures/exfoo.expr" exFoo
