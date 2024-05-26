{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}

-- \| Circles dataset and gradient descent in a multilayer neural network
--
-- 1. Install stack (command line interface is marked by $):
--   $ wget -qO- https://get.haskellstack.org/ | sh
-- (alternatively, curl -sSL https://get.haskellstack.org/ | sh)
--
-- 2. Install open-blas from https://www.openblas.net/
-- (needed for hmatrix package)
--
-- 3. Compile and run
--
--   $ stack --resolver lts-10.6 --install-ghc ghc --package hmatrix-0.18.2.0 --package hmatrix-morpheus-0.1.1.2 -- -O2 Main.hs
--   $ ./Main

import Data.Bifunctor (Bifunctor (first))
import Data.Massiv.Array (Comp (ParN), Ix2 (Ix2), Load (makeArray), Size (size), Sz (Sz1, Sz2), U, append', applyStencil, compute, defRowMajor, dropWindow, expandOuter, makeSplitSeedArray, makeStencil, noPadding, toList, uniformRangeArray, (!+!))
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy (Default (def), blue, layout_title, opaque, orange, plot, points, setColors, (.=))
import NeuralNetwork
  ( Activation (Relu, Sigmoid),
    Mode (TrainMode),
    NeuralNetworkConfig (NeuralNetworkConfig),
    RunNet (Train),
    accuracy,
    adamParams,
    genNetwork,
    optimize,
    optimizeAdam,
  )
import Statistics.Distribution (ContGen (genContVar))
import Statistics.Distribution.Normal (standard)
import System.Random (RandomGen (split), newStdGen)
import System.Random.Stateful (runStateGen)
import Text.Printf (printf)

genNoise :: (RandomGen g) => Double -> g -> (Double, g)
genNoise noise g = first (noise *) . runStateGen g $ genContVar standard

-- | Circles dataset
makeCircles :: Int -> Double -> Double -> IO (RunNet TrainMode Double)
makeCircles m factor noise = do
  (g, g') <- split <$> newStdGen
  let makeFeatures factor' size' =
        expandOuter @U
          (Sz1 2)
          ( \r -> \case
              0 -> factor' * cos r
              1 -> factor' * sin r
              _ -> error "impossible: !!!"
          )
          . compute
          $ uniformRangeArray g (0, 2 * pi) (ParN 4) (Sz1 size')
      features = compute $ append' 1 (makeFeatures 1 $ m `div` 2) (makeFeatures factor $ m - m `div` 2)
      noises = compute $ makeSplitSeedArray defRowMajor g' split (ParN 4) (Sz2 2 m) (\_ _ g'' -> genNoise noise g'')
      targets = makeArray (ParN 4) (Sz2 1 m) $ \(Ix2 _ j) -> if j <= m `div` 2 then 0 else 1
  pure $ Train (features !+! noises) targets

-- | Spirals dataset.
-- Note, produces twice more points than m.
-- makeSpirals ::
--   Int -> Double -> IO (RunNet TrainMode Double)
-- makeSpirals m noise = do
--   r0 <- (M.map (780 * 2 * pi / 360) . sqrt) <$> rand (0, 1) (Sz2 m 1)
--   let makeNoise = M.map (noise *) <$> rand (0, 1) (Sz2 m 1)
--   d1x0 <- makeNoise
--   d1y0 <- makeNoise

--   let d1x = d1x0 - cos (r0) * r0
--   let d1y = d1y0 + sin (r0) * r0

--   let x = (fromBlocks [[d1x, d1y], [-d1x, -d1y]]) / 10.0
--   let y1 = m >< 1 $ repeat 0
--   let y2 = m >< 1 $ repeat 1
--   let y = y1 === y2
--   return $ Train x y

drawPoints :: String -> RunNet TrainMode Double -> IO ()
drawPoints name (Train inputs tgts) = do
  let inputs' =
        toList . dropWindow $ applyStencil noPadding (makeStencil (Sz2 2 1) (Ix2 0 0) (\get -> (get $ Ix2 0 0, get $ Ix2 1 0))) inputs
      tgts' = toList tgts
      pointsByClass [] [] r = r
      pointsByClass [] _ _ = error "impossible: not enough target"
      pointsByClass _ [] _ = error "impossible: not enough input"
      pointsByClass (x : xs) (y : ys) (as, bs)
        | y == 0 = pointsByClass xs ys (x : as, bs)
        | y == 1 = pointsByClass xs ys (as, x : bs)
        | otherwise = error "impossible: y not 0 or 1"
      (ps1, ps2) = pointsByClass inputs' tgts' ([], [])
  toFile def name $ do
    layout_title .= "train_circles"
    setColors $ [opaque orange, opaque blue]
    plot $ points "1" ps1
    plot $ points "2" ps2

experiment1 :: (RandomGen g) => g -> IO ()
experiment1 g = do
  trainSet <- makeCircles 200 0.6 0.1
  testSet <- makeCircles 100 0.6 0.1

  drawPoints "train_circle.svg" trainSet

  let net = genNetwork g $ NeuralNetworkConfig 2 [(128, Relu), (1, Sigmoid)]
      epochs = 1000
      lr = 0.001 -- Learning rate
      net' = optimize lr epochs net trainSet
      netA = optimizeAdam adamParams epochs net trainSet

  putStrLn $ printf "Circles problem, 1 hidden layer of 128 neurons, %d epochs" epochs
  putStrLn "---"

  putStrLn $ printf "Training accuracy (gradient descent) %.1f" (net' `accuracy` trainSet)
  putStrLn $ printf "Validation accuracy (gradient descent) %.1f\n" (net' `accuracy` testSet)

  putStrLn $ printf "Training accuracy (Adam) %.1f" (netA `accuracy` trainSet)
  putStrLn $ printf "Validation accuracy (Adam) %.1f\n" (netA `accuracy` testSet)

-- experiment2 :: IO ()
-- experiment2 = do
--   trainSet <- makeSpirals 200 0.5
--   testSet <- makeSpirals 100 0.5
--   -- saveMatrix "/tmp/spir.x" "%g" dta
--   -- saveMatrix "/tmp/spir.y" "%g" tgt

--   drawPoints "train_spiral.svg" trainSet
--   let epochs = 700

--   putStrLn $ printf "Spirals problem, Adam, %d epochs" epochs
--   putStrLn "---"
--   putStrLn "1 hidden layer, 128 neurons (513 parameters)"
--   net0 <- genNetwork $ NeuralNetworkConfig 2 [(128, Relu), (1, Id)]
--   let net0' = optimizeAdam adamParams epochs net0 trainSet

--   putStrLn $ printf "Training accuracy %.1f" (net0' `accuracy` trainSet)
--   putStrLn $ printf "Validation accuracy %.1f\n" (net0' `accuracy` testSet)

--   putStrLn "1 hidden layer, 512 neurons (2049 parameters)"
--   net1 <- genNetwork $ NeuralNetworkConfig 2 [(512, Relu), (1, Id)]
--   let net1' = optimizeAdam adamParams epochs net1 trainSet

--   putStrLn $ printf "Training accuracy %.1f" (net1' `accuracy` trainSet)
--   putStrLn $ printf "Validation accuracy %.1f\n" (net1' `accuracy` testSet)

--   putStrLn "3 hidden layers, 40, 25, and 10 neurons (1416 parameters)"
--   net2 <- genNetwork $ NeuralNetworkConfig 2 [(40, Relu), (25, Relu), (10, Relu), (1, Id)]
--   let net2' = optimizeAdam adamParams epochs net2 trainSet

--   putStrLn $ printf "Training accuracy %.1f" (net2' `accuracy` trainSet)
--   putStrLn $ printf "Validation accuracy %.1f\n" (net2' `accuracy` testSet)

main :: IO ()
main = do
  g <- newStdGen
  print g
  experiment1 g

-- >> experiment2
