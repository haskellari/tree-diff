{-# OPTIONS -fno-warn-orphans #-}
module Main (main) where

import Control.DeepSeq   (NFData (..))
import Control.Exception (evaluate)
import Criterion.Main    (bench, bgroup, defaultMain, nf)

import qualified Data.Algorithm.Diff as Diff

import Data.TreeDiff.List (diffBy)

smallA :: [Int]
smallB :: [Int]

smallA = [0, 5 .. 100]
smallB = [0, 3 .. 72]

bigA :: [Int]
bigA = [0, 5 .. 10000]

bigB :: [Int]
bigB = [0, 3 .. 7200]

main :: IO ()
main = do
  evaluate (rnf smallA)
  evaluate (rnf smallB)

  evaluate (rnf bigA)
  evaluate (rnf bigB)

  defaultMain
    [ bgroup "same"
      [ bgroup "small"
        [ bench "lib" $ nf (uncurry (diffBy (==))) (smallA, smallA)
        , bench "Diff" $ nf (uncurry (Diff.getDiffBy (==))) (smallA, smallA)
        ]
      , bgroup "big"
        [ bench "lib" $ nf (uncurry (diffBy (==))) (bigA, bigA)
        , bench "Diff" $ nf (uncurry (Diff.getDiffBy (==))) (bigA, bigA)
        ]
      ]
    , bgroup "different"
      [ bgroup "small"
        [ bench "lib" $ nf (uncurry (diffBy (==))) (smallA, smallB)
        , bench "Diff" $ nf (uncurry (Diff.getDiffBy (==))) (smallA, smallB)
        ]
      , bgroup "big"
        [ bench "lib" $ nf (uncurry (diffBy (==))) (bigA, bigB)
        , bench "Diff" $ nf (uncurry (Diff.getDiffBy (==))) (bigA, bigB)
        ]
      ]
    ]

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

instance (NFData a, NFData b) => NFData (Diff.PolyDiff a b) where
    rnf (Diff.First x)  = rnf x
    rnf (Diff.Second y) = rnf y
    rnf (Diff.Both x y) = rnf x `seq` rnf y
