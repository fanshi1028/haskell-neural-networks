{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
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
import Foreign (Storable)
import GHC.Natural (Natural)
import Numeric.LinearAlgebra as LA
  ( Linear (scale),
    Matrix,
    Transposable (tr'),
    cmap,
    cols,
    matrix,
    randn,
    rows,
    sumElements,
    toColumns,
    (<>),
    (><),
  )

-- Activation function:
data Activation = Relu | Sigmoid | Tanh | Id

-- Neural network layer: weights, biases, and activation
data Layer a = Layer !(Matrix a) !(Matrix a) !Activation

data NeuralNetworkConfig = NeuralNetworkConfig !Int ![(Int, Activation)]

type NeuralNetwork a = [Layer a]

-- | Weight and bias gradients
data Gradients a = Gradients !(Matrix a) !(Matrix a)

-- | Lookup activation function by a symbol
getActivation :: Activation -> (Matrix Double -> Matrix Double)
getActivation = \case
  Id -> id
  Sigmoid -> cmap $ \x -> recip (1.0 + exp (-x))
  Relu -> cmap $ max 0
  Tanh -> cmap $ \x -> 2 * recip (1 + exp (-2 * x)) - 1

filledOne :: (Storable a, Num a) => Matrix t -> Matrix a
filledOne z = rows z >< cols z $ repeat 1

-- | Lookup activation function derivative by a symbol
getActivation' :: Activation -> Matrix Double -> (Matrix Double -> Matrix Double)
getActivation' Id _ = id
getActivation' Sigmoid (getActivation Sigmoid -> z) = (z * (filledOne z - z) *)
getActivation' Relu (cmap (\z -> if z >= 0 then 1 else 0) -> z) = (z *)
getActivation' Tanh (getActivation Tanh -> z) = ((filledOne z - cmap (^ (2 :: Integer)) z) *)

data Mode = TrainMode | InferMode

data RunNet (mode :: Mode) r where
  Train :: (Matrix r) -> (Matrix r) -> RunNet TrainMode r
  Infer :: (Matrix r) -> RunNet InferMode r

-- | Both forward and backward neural network passes
pass ::
  -- | `NeuralNetwork` `Layer`s: weights and activations
  NeuralNetwork Double ->
  -- | Data set
  RunNet s Double ->
  -- | NN computation from forward pass and weights gradients
  (Matrix Double, [Gradients Double])
pass net run = snd . _pass net $ case run of
  Train x _ -> x
  Infer x -> x
  where
    _pass [] inp =
      let prediction' = getActivation Sigmoid inp
          -- Gradient of cross-entropy loss
          -- after sigmoid activation.
          mLoss = case run of
            Train _ target -> Just $ prediction' - target
            Infer _ -> Nothing
       in (mLoss, (prediction', []))
    _pass (Layer w b sact : layers) inp =
      let lin = (inp LA.<> w) + b
          y = getActivation sact lin
          (mDZ, (prediction', gradients)) = _pass layers y
       in case mDZ of
            Nothing -> (Nothing, (prediction', []))
            Just dZ -> (Just dX, (prediction', Gradients dW dB : gradients))
              where
                dY = getActivation' sact lin dZ
                dW = cmap (/ (fromIntegral $ rows inp)) (tr' inp LA.<> dY)
                dB = cmap (/ (fromIntegral $ rows dY)) $ matrix (cols dY) $ map sumElements (toColumns dY)
                dX = dY LA.<> tr' w

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
      f (Layer w b act) (Gradients dW dB) = Layer (w - lr `scale` dW) (b - lr `scale` dB) act

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
    zerosLike m = matrix c (replicate (r * c) 0.0)
      where
        r = rows m
        c = cols m
    (w, _, _) = _adam p iterN (w0, s0, v0) dataSet

_adam ::
  AdamParameters ->
  Natural ->
  ([Layer Double], [(Matrix Double, Matrix Double)], [(Matrix Double, Matrix Double)]) ->
  RunNet TrainMode Double ->
  ([Layer Double], [(Matrix Double, Matrix Double)], [(Matrix Double, Matrix Double)])
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
              (Matrix Double, Matrix Double) ->
              (Matrix Double, Matrix Double) ->
              Layer Double
            f (Layer w_ b_ sf) (vW, vB) (sW, sB) =
              Layer
                (w_ - lr `scale` vW / ((sqrt sW) `addC` epsilon))
                (b_ - lr `scale` vB / ((sqrt sB) `addC` epsilon))
                sf

            addC m c = cmap (+ c) m

            f2 ::
              (Matrix Double, Matrix Double) ->
              Gradients Double ->
              (Matrix Double, Matrix Double)
            f2 (sW, sB) (Gradients dW dB) =
              ( beta2 `scale` sW + (1 - beta2) `scale` (dW ^ (2 :: Integer)),
                beta2 `scale` sB + (1 - beta2) `scale` (dB ^ (2 :: Integer))
              )

            f3 ::
              (Matrix Double, Matrix Double) ->
              Gradients Double ->
              (Matrix Double, Matrix Double)
            f3 (vW, vB) (Gradients dW dB) =
              ( beta1 `scale` vW + (1 - beta1) `scale` dW,
                beta1 `scale` vB + (1 - beta1) `scale` dB
              )

-- | Perform a binary classification
inferBinary ::
  NeuralNetwork Double -> Matrix Double -> Matrix Double
inferBinary net dta = cmap (\a -> if a < 0.5 then 0 else 1) . fst . pass net $ Infer dta

-- | Generate random weights and biases
genWeights :: (Int, Int) -> IO (Matrix Double, Matrix Double)
genWeights (nin, nout) = do
  w <- _genWeights (nin, nout)
  b <- _genWeights (1, nout)
  return (w, b)
  where
    _genWeights (nin', nout') = do
      let k = sqrt (1.0 / fromIntegral nin')
      w <- randn nin' nout'
      return (k `scale` w)

-- | Generate a neural network with random weights
genNetwork :: NeuralNetworkConfig -> IO (NeuralNetwork Double)
genNetwork (NeuralNetworkConfig nStart l) = flip para ((nStart, undefined) :| l) $ \case
  NonEmptyF (nIn, _) mr -> case mr of
    Nothing -> pure []
    Just ((nOut, activation) :| _, mLayers) -> do
      layers <- mLayers
      (w, b) <- genWeights (nIn, nOut)
      pure $ Layer w b activation : layers

-- | Binary classification accuracy in percent
accuracy ::
  -- | Neural network
  [Layer Double] ->
  -- | Dataset
  RunNet TrainMode Double ->
  Double
accuracy net (Train dta tgt) = 100 * (1 - e / m)
  where
    e = sumElements $ abs (tgt - net `inferBinary` dta)
    m = fromIntegral $ rows tgt
