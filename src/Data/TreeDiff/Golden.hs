{-# LANGUAGE OverloadedStrings #-}
-- | "Golden tests" using 'ediff' comparison.
module Data.TreeDiff.Golden (
    ediffGolden,
    ediffGolden1,
) where

import Data.TreeDiff
import System.Console.ANSI (SGR (Reset), setSGRCode)
import Text.Parsec         (eof, parse)
import Text.Parsec.Text ()

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Prettyprinter
                 (LayoutOptions (LayoutOptions, layoutPageWidth),
                 PageWidth (AvailablePerLine), layoutPretty, unAnnotate)
import           Prettyprinter.Render.Terminal (renderStrict)

-- | Make a golden tests.
--
-- 'ediffGolden' is testing framework agnostic, thus the type
-- looks intimidating.
--
-- An example using @tasty-golden@,
-- 'goldenTest' is imported from "Test.Tasty.Golden.Advanced"
--
-- @
-- exTest :: TestTree
-- exTest = 'ediffGolden' goldenTest "golden test" "fixtures/ex.expr" $
--    action constructing actual value
-- @
--
-- The 'ediffGolden' will read an 'Expr' from provided path to golden file,
-- and compare it with a 'toExpr' of a result. If values differ,
-- the (compact) diff of two will be printed.
--
-- See <https://github.com/phadej/tree-diff/blob/master/tests/Tests.hs>
-- for a proper example.
--
ediffGolden
    :: (Eq a, ToExpr a)
    => (testName -> IO Expr -> IO Expr -> (Expr -> Expr -> IO (Maybe String)) -> (Expr -> IO ()) -> testTree) -- ^ 'goldenTest'
    -> testName  -- ^ test name
    -> FilePath  -- ^ path to "golden file"
    -> IO a      -- ^ result value
    -> testTree
ediffGolden impl testName fp x = ediffGolden1 impl' testName fp (\() -> x) where
    impl' n expect actual = impl n expect (actual ())

-- | Like 'ediffGolden1' but with an additional argument for generation of actual value.
--
-- @since 0.3.2
--
ediffGolden1
    :: (Eq a, ToExpr a)
    => (testName -> IO Expr -> (arg -> IO Expr) -> (Expr -> Expr -> IO (Maybe String)) -> (Expr -> IO ()) -> testTree) -- ^ 'goldenTest'
    -> testName  -- ^ test name
    -> FilePath  -- ^ path to "golden file"
    -> (arg -> IO a)      -- ^ result value
    -> testTree
ediffGolden1 impl testName fp x = impl testName expect actual cmp wrt
  where
    actual arg = fmap toExpr (x arg)
    expect = do
        contents <- BS.readFile fp
        case parse (exprParser <* eof) fp $ TE.decodeUtf8 contents of
            Left err -> print err >> fail "parse error"
            Right r  -> return r
    cmp a b
        | a == b    = return Nothing
        | otherwise = return $ Just $
            setSGRCode [Reset] ++ T.unpack (render $ ansiWlEditExprCompact $ ediff a b)
    wrt expr = BS.writeFile fp $ TE.encodeUtf8 $ render (unAnnotate (ansiWlExpr expr)) `T.append` "\n"
    render = renderStrict . layoutPretty LayoutOptions {layoutPageWidth=AvailablePerLine 80 0.4}
