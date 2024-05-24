{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Fully-connected neural network
module NeuralNetwork
  ( NeuralNetwork,
    NeuralNetworkConfig (..),
    Layer (..),
    Activation (..),
    Mode (..),
    RunNet (..),
    genNetwork,

    -- * Training
    optimize,
    AdamParameters (..),
    adamParams,
    optimizeAdam,

    -- * Inference
    inferBinary,
    accuracy,
  )
where

import Data.Functor.Base (NonEmptyF (NonEmptyF))
import Data.Functor.Foldable (Recursive (cata, para))
import Data.List.NonEmpty as NE (NonEmpty ((:|)))
import Data.Massiv.Array (Comp (ParN), Dimension (Dim2), Ix2 (Ix2), Load (makeArray), Matrix, NumericFloat, Size (size), Sz (Sz1, Sz2), U (U), Unbox, absA, applyStencil, avgStencil, compute, computeAs, defRowMajor, expA, expandWithin, extract', makeSplitSeedArray, negateA, noPadding, recipA, sqrtA, transpose, (!), (!*!), (!+!), (!-!), (!/!), (!>), (!><!), (*.), (+.), (-.), (.+), (.-))
import Data.Massiv.Array qualified as A (map)
import Data.Massiv.Array.Numeric ()
import GHC.Natural (Natural)
import Statistics.Distribution (ContGen (genContVar))
import Statistics.Distribution.Normal (standard)
import System.Random (RandomGen (split))
import System.Random.Stateful (runStateGen)

-- Activation function:
data Activation = Relu | Sigmoid | Tanh | Id deriving (Show)

-- Neural network layer: weights, biases, and activation
data Layer a
  = Layer !(Matrix U a) !(Matrix U a) !Activation
  deriving (Show)

-- \| Batchnorm1d (Vector a) (Vector a) (Vector a) (Vector a)

data NeuralNetworkConfig = NeuralNetworkConfig !Int ![(Int, Activation)]

type NeuralNetwork a = [Layer a]

-- | Weight and bias gradients
data Gradients a = Gradients !(Matrix U a) !(Matrix U a)

instance (Unbox e, Floating e) => NumericFloat U e

-- | Lookup activation function by a symbol
getActivation :: Activation -> (Matrix U Double -> Matrix U Double)
getActivation = \case
  Id -> id
  Sigmoid -> \x -> recipA (1.0 +. expA (negateA x))
  Relu -> compute . A.map (max 0)
  Tanh -> \x -> 2 *. recipA (1 +. expA (negateA $ 2 *. x)) .- 1

-- | Lookup activation function derivative by a symbol
getActivation' :: Activation -> Matrix U Double -> (Matrix U Double -> Matrix U Double)
-- getActivation' Id _ = id
getActivation' Id _ = undefined -- ??
getActivation' Sigmoid (getActivation Sigmoid -> z) = (z !*! (1 -. z) !*!)
getActivation' Relu (compute . A.map (\z -> if z >= 0 then 1 else 0) -> z) = (z !*!)
getActivation' Tanh (getActivation Tanh -> z) = ((compute $ 1 -. (A.map (^ (2 :: Integer)) z)) !*!)

data Mode = TrainMode | InferMode

data RunNet (mode :: Mode) r where
  Train :: (Matrix U r) -> (Matrix U r) -> RunNet TrainMode r
  Infer :: (Matrix U r) -> RunNet InferMode r

-- | Both forward and backward neural network passes
pass ::
  -- | `NeuralNetwork` `Layer`s: weights and activations
  NeuralNetwork Double ->
  -- | Data set
  RunNet s Double ->
  -- | NN computation from forward pass and weights gradients
  (Matrix U Double, [Gradients Double])
pass net run = snd . _pass net $ case run of
  Train x _ -> x
  Infer x -> x
  where
    _pass [] prediction' =
      let -- Gradient of cross-entropy loss
          -- after sigmoid activation.
          mLoss = case run of
            Train _ target -> Just $ prediction' !-! target
            Infer _ -> Nothing
       in (mLoss, (prediction', []))
    _pass (Layer w b sact : layers) inp =
      let Sz2 inpR _ = size inp
          lin = (inp !><! w) !+! (compute $ expandWithin Dim2 (Sz1 inpR) (\v _ -> v) (b !> 0))
          y = compute $ getActivation sact lin
          (mDZ, (prediction', gradients)) = _pass layers y
       in case mDZ of
            Nothing -> (Nothing, (prediction', []))
            Just dZ -> (Just dX, (prediction', Gradients dW dB : gradients))
              where
                dY = compute $ getActivation' sact lin dZ
                dW = compute $ A.map (/ fromIntegral inpR) $ compute (transpose inp) !><! dY
                Sz2 dYR _ = size dY
                dB = compute $ applyStencil noPadding (avgStencil $ Sz2 dYR 1) dY
                dX = dY !><! compute (transpose w)

-- | Gradient descent optimization
optimize ::
  -- | Learning rate
  Double ->
  -- | No of iterations
  Natural ->
  -- | Neural network
  NeuralNetwork Double ->
  -- | Dataset
  RunNet TrainMode Double ->
  -- | Updated neural network
  NeuralNetwork Double
optimize lr iterN net runNet = flip cata iterN $ \case
  Nothing -> net
  Just net' -> zipWith f net' . snd $ pass net' runNet
    where
      f (Layer w b act) (Gradients (compute -> dW) (compute -> dB)) = Layer (w !-! lr *. dW) (b !-! lr *. dB) act

data AdamParameters = AdamParameters
  { _beta1 :: !Double,
    _beta2 :: !Double,
    _epsilon :: !Double,
    _lr :: !Double
  }

-- | Adam optimizer parameters
adamParams :: AdamParameters
adamParams =
  AdamParameters
    { _beta1 = 0.9,
      _beta2 = 0.999,
      _epsilon = 1e-8,
      _lr = 0.001
    }

-- \^ Learning rate

-- | Adam optimization
optimizeAdam ::
  -- | Adam parameters
  AdamParameters ->
  -- | No of iterations
  Natural ->
  -- | Neural network layers
  NeuralNetwork Double ->
  -- | Dataset
  RunNet TrainMode Double ->
  NeuralNetwork Double
optimizeAdam p iterN w0 dataSet = w
  where
    s0 = map zf w0
    v0 = map zf w0
    zf (Layer a b _) = (zerosLike a, zerosLike b)
    zerosLike m = makeArray (ParN 4) (size m) $ \_ -> 0
    (w, _, _) = _adam p iterN (w0, s0, v0) dataSet

_adam ::
  AdamParameters ->
  Natural ->
  ([Layer Double], [(Matrix U Double, Matrix U Double)], [(Matrix U Double, Matrix U Double)]) ->
  RunNet TrainMode Double ->
  ([Layer Double], [(Matrix U Double, Matrix U Double)], [(Matrix U Double, Matrix U Double)])
_adam
  AdamParameters
    { _lr = lr,
      _beta1 = beta1,
      _beta2 = beta2,
      _epsilon = epsilon
    }
  iterN
  (w0, s0, v0)
  dataSet = flip cata iterN $ \case
    Nothing -> (w0, s0, v0)
    Just r -> go r
      where
        go (w, s, v) = (wN, sN, vN)
          where
            (_, gradients) = pass w dataSet

            sN = zipWith f2 s gradients
            vN = zipWith f3 v gradients
            wN = zipWith3 f w vN sN

            f ::
              Layer Double ->
              (Matrix U Double, Matrix U Double) ->
              (Matrix U Double, Matrix U Double) ->
              Layer Double
            f (Layer w_ b_ sf) (vW, vB) (sW, sB) =
              Layer
                (w_ !-! (compute $ lr *. vW !/! ((sqrtA sW) .+ epsilon)))
                (b_ !-! (compute $ lr *. vB !/! ((sqrtA sB) .+ epsilon)))
                sf

            f2 ::
              (Matrix U Double, Matrix U Double) ->
              Gradients Double ->
              (Matrix U Double, Matrix U Double)
            f2 (sW, sB) (Gradients dW dB) =
              ( beta2 *. sW !+! (1 - beta2) *. (dW !*! dW),
                beta2 *. sB !+! (1 - beta2) *. (dB !*! dB)
              )

            f3 ::
              (Matrix U Double, Matrix U Double) ->
              Gradients Double ->
              (Matrix U Double, Matrix U Double)
            f3 (vW, vB) (Gradients dW dB) =
              ( beta1 *. vW !+! (1 - beta1) *. dW,
                beta1 *. vB !+! (1 - beta1) *. dB
              )

-- | Generate a neural network with random weights
genNetwork :: (RandomGen g) => g -> NeuralNetworkConfig -> NeuralNetwork Double
genNetwork g (NeuralNetworkConfig nStart l) =
  fst . flip para ((nStart, undefined) :| l) $ \case
    NonEmptyF (nIn, _) mr -> case mr of
      Nothing -> ([], g)
      Just ((nOut, activation) :| _, (layers, split -> (g', g''))) ->
        let a = computeAs U $ makeSplitSeedArray defRowMajor g' split (ParN 4) (Sz2 (nIn + 1) nOut) $ \_ _ g''' -> runStateGen g''' (genContVar standard)
            w = compute $ extract' (Ix2 0 0) (Sz2 nIn nOut) a
            b = compute $ extract' (Ix2 (nIn - 1) 0) (Sz2 1 nOut) a
         in (Layer w b activation : layers, g'')

-- | Perform a binary classification
inferBinary ::
  NeuralNetwork Double -> Matrix U Double -> Matrix U Double
inferBinary net dta = compute $ A.map (\a -> if a < 0.5 then 0 else 1) . fst . pass net $ Infer dta

-- | Binary classification accuracy in percent
accuracy ::
  -- | Neural network
  [Layer Double] ->
  -- | Dataset
  RunNet TrainMode Double ->
  Double
accuracy net (Train dta tgt) = 100 * (1 - (avg ! (Ix2 0 0)))
  where
    a = absA $ tgt !-! net `inferBinary` dta
    avg = compute @U $ applyStencil noPadding (avgStencil $ size a) a
