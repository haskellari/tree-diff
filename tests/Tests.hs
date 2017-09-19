module Main where

import Data.TreeDiff

import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain $ testGroup "tests"
    [
    ]
