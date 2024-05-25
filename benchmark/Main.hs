{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Comonad.Cofree (Cofree ((:<)))
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Functor.Foldable (histo)
import GHC.Natural (Natural)

-- The function we're benchmarking.
fib :: Natural -> Natural
fib m = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)

-- The function we're benchmarking.
fib2 :: Natural -> Natural
fib2 = histo $ \case
  Nothing -> 0
  Just (_ :< Nothing) -> 1
  Just (a :< Just (b :< _)) -> a + b

-- Our benchmark harness.
main :: IO ()
main =
  defaultMain
    [ bgroup
        "fib"
        [ bench "1" $ whnf fib 1,
          bench "5" $ whnf fib 5,
          bench "9" $ whnf fib 9,
          bench "11" $ whnf fib 11
        ],
      bgroup
        "fib2"
        [ bench "1" $ whnf fib2 1,
          bench "5" $ whnf fib2 5,
          bench "9" $ whnf fib2 9,
          bench "11" $ whnf fib2 11
        ]
    ]
