module Data.TreeDiff.Golden (
    ediffGolden,
    ) where

import Data.TreeDiff
import System.Console.ANSI (SGR (Reset), setSGRCode)
import Prelude ()
import Prelude.Compat
import Text.Trifecta              (eof, parseFromFileEx)
import Text.Trifecta.Result       (ErrInfo (..), Result (..))

import qualified Data.Text    as T
import qualified Data.Text.IO as T

ediffGolden
    :: (Eq a, ToExpr a)
    => (testName -> IO Expr -> IO Expr -> (Expr -> Expr -> IO (Maybe String)) -> (Expr -> IO ()) -> testTree) -- ^ 'goldenTest'
    -> testName  -- ^ test name
    -> FilePath  -- ^ path to "golden file"
    -> a         -- ^ result value
    -> testTree
ediffGolden impl testName fp x = impl testName expect actual cmp wrt
  where
    actual = return (toExpr x)
    expect = do
        result <- parseFromFileEx (exprParser <* eof) fp
        case result of
            Failure err -> print (_errDoc err) >> fail "parse error"
            Success r   -> return r
    cmp a b
        | a == b    = return $ Nothing
        | otherwise = return $ Just $ 
            setSGRCode [Reset] ++ show (ansiWlEditExpr $ ediff a b)
    wrt expr = T.writeFile fp $ T.pack $ show (prettyExpr expr) ++ "\n"
