-- | "Golden tests" using 'ediff' comparison.
module Data.TreeDiff.Golden (
    ediffGolden,
    ) where

import Data.TreeDiff
import Prelude ()
import Prelude.Compat
import System.Console.ANSI (SGR (Reset), setSGRCode)
import Text.Parsec         (eof, parse)
import Text.Parsec.Text ()

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- | Make a golden tests.
--
-- 'ediffGolden' is testing framework agnostic, thus the test framework
-- looks intimdating.
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
-- the diff of two will be printed.
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
ediffGolden impl testName fp x = impl testName expect actual cmp wrt
  where
    actual = fmap toExpr x
    expect = do
        contents <- T.readFile fp
        case parse (exprParser <* eof) fp contents of
            Left err -> print err >> fail "parse error"
            Right r  -> return r
    cmp a b
        | a == b    = return $ Nothing
        | otherwise = return $ Just $
            setSGRCode [Reset] ++ show (ansiWlEditExpr $ ediff a b)
    wrt expr = T.writeFile fp $ T.pack $ show (prettyExpr expr) ++ "\n"
