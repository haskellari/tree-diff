{-# LANGUAGE DeriveGeneric     #-}
module Main (main) where

import Data.List.Compat (sortOn)
import Data.Proxy                 (Proxy (..))
import Data.TreeDiff
import Data.TreeDiff.List
import Data.TreeDiff.Golden
import Data.TreeDiff.QuickCheck
import qualified Data.Vector      as V
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

import Criterion.Main.Options(defaultConfig)
import Criterion.Types(Config(..))
import qualified Criterion.Main as CM -- (defaultMainWith, whnf, bench)

myConfig = defaultConfig {
              -- Resample 10 times for bootstrapping
              resamples = 1000
            , csvFile=Just "tests.csv"
           }

listsa = [0, 5 .. 100]
listsb = [0, 3 .. 72]

listba = [0, 5 .. 10000]
listbb = [0, 3 .. 7200]

main :: IO ()
main = do
  CM.defaultMainWith myConfig [
      CM.bgroup "small" [
          CM.bench "newDiff" $ CM.whnf (uncurry (diffBy (==))) (listsa, listsb)
        , CM.bench "oldDiff" $ CM.whnf (uncurry (oldDiffBy (==))) (listsa, listsb)
      ]
    , CM.bgroup "big" [
          CM.bench "newDiff" $ CM.whnf (uncurry (diffBy (==))) (listba, listbb)
        , CM.bench "oldDiff" $ CM.whnf (uncurry (oldDiffBy (==))) (listba, listbb)
      ]
    ]
  defaultMain $ testGroup "tests"
    [ testProperty "trifecta-pretty roundtrip" roundtripTrifectaPretty
    , testProperty "parsec-ansi-wl-pprint roundtrip" roundtripParsecAnsiWl
    , testProperty "check validity new diffBy" checkEquivalentDiffBy
    , goldenTests
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

checkEquivalentDiffBy :: [Int] -> [Int] -> Bool
checkEquivalentDiffBy xs ys = oldDiffBy (==) xs ys == diffBy (==) xs ys

oldDiffBy :: (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
oldDiffBy eq xs' ys' = reverse (snd (lcs xn yn))
  where
    xn = V.length xs
    yn = V.length ys

    xs = V.fromList xs'
    ys = V.fromList ys'

    memo = V.fromList
        [ impl xi yi
        | xi <- [0 .. xn]
        , yi <- [0 .. yn]
        ]

    lcs xi yi = memo V.! (yi + xi * (yn + 1))

    impl 0 0 = (0, [])
    impl 0 m = case lcs 0 (m-1) of
        (w, edit) -> (w + 1, Ins (ys V.! (m - 1)) : edit)
    impl n 0 = case lcs (n -1) 0 of
        (w, edit) -> (w + 1, Del (xs V.! (n - 1)) : edit)

    impl n m = head $ sortOn fst
        [ edit
        , bimap (+1) (Ins y :) (lcs n (m - 1))
        , bimap (+1) (Del x :) (lcs (n - 1) m)
        ]
      where
        x = xs V.! (n - 1)
        y = ys V.! (m - 1)

        edit
            | eq x y    = bimap id   (Cpy x :)   (lcs (n - 1) (m - 1))
            | otherwise = bimap (+1) (Swp x y :) (lcs (n -1 ) (m - 1))

bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimap f g (x, y) = (f x, g y)

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
    , fooNew :: Bool
    , fooStr :: String
    }
  deriving (Eq, Show, Generic)

instance ToExpr Foo

exFoo :: Foo
exFoo = Foo
    { fooInt = 42
    , fooBar = [Just "pub", Just "night\nclub"]
    , fooQuu = (125.375, Proxy)
    , fooNew = True
    , fooStr = "Some Name"
    }

newtype MyInt1 = MyInt1 Int
  deriving (Eq, Show, Generic)

newtype MyInt2 = MyInt2 { getMyInt2 :: Int }
  deriving (Eq, Show, Generic)

data MyInt3 = MyInt3 { getMyInt3 :: Int}
  deriving (Eq, Show, Generic)

data Positional = Positional Int Bool Char
  deriving (Eq, Show, Generic)

data Empty
  deriving (Generic)

instance Eq Empty where
    _ == _ = True

instance Show Empty where
    showsPrec _ _ = error "Empty?"

instance ToExpr MyInt1
instance ToExpr MyInt2
instance ToExpr MyInt3
instance ToExpr Positional
instance ToExpr Empty

goldenTests :: TestTree
goldenTests = testGroup "Golden"
    [ ediffGolden goldenTest "exFoo" "fixtures/exfoo.expr" $
        return exFoo
    , ediffGolden goldenTest "MyInt1" "fixtures/MyInt1.expr" $
        return $ MyInt1 42
    , ediffGolden goldenTest "MyInt2" "fixtures/MyInt2.expr" $
        return $ MyInt2 42
    , ediffGolden goldenTest "MyInt3" "fixtures/MyInt3.expr" $
        return $ MyInt3 42
    , ediffGolden goldenTest "Positional" "fixtures/Positional.expr" $
        return $ Positional 12 True 'z'
    ]

