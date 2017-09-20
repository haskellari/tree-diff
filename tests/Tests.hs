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

import qualified Text.Parsec                  as P
import qualified Text.PrettyPrint.ANSI.Leijen as WL
import qualified Text.Trifecta                as T (eof, parseString)
import qualified Text.Trifecta.Result         as T (ErrInfo (..), Result (..))

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ testProperty "trifecta-pretty roundtrip" roundtripTrifectaPretty
    , testProperty "parsec-ansi-wl-pprint roundtrip" roundtripParsecAnsiWl
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
roundtripTrifectaPretty :: Expr -> Property
roundtripTrifectaPretty e = counterexample info $ ediffEq (Just e) res'
  where
    doc = show (prettyExpr e)
    res = T.parseString (exprParser <* T.eof) mempty doc

    info = case res of
        T.Success e'  ->
            doc
            ++ "\n" ++
            show e'
        T.Failure err ->
            doc
            ++ "\n" ++
            show (T._errDoc err)

    res' = case res of
        T.Success e' -> Just e'
        T.Failure _  -> Nothing

roundtripParsecAnsiWl :: Expr -> Property
roundtripParsecAnsiWl e = counterexample info $ ediffEq (Just e) res'
  where
    doc = show (WL.plain (ansiWlExpr e))
    res = P.parse (exprParser <* P.eof) "<memory>" doc

    info = case res of
        Right e'  ->
            doc
            ++ "\n" ++
            show e'
        Left err ->
            doc
            ++ "\n" ++
            show err

    res' = either (const Nothing) Just res

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
exFooTests = ediffGolden goldenTest "golden exFoo" "fixtures/exfoo.expr" $
    return exFoo
