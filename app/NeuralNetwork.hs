{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

import Data.Bifunctor (Bifunctor (first, second))
import Data.Functor.Base (NonEmptyF (NonEmptyF))
import Data.Functor.Foldable (Recursive (para))
import Data.List.NonEmpty as NE (NonEmpty ((:|)))
import Data.Massiv.Array (Comp (ParN), Dimension (Dim1), Ix2 (Ix2), Load (makeArray), Matrix, NumericFloat, Size (size), Sz (Sz1, Sz2), U (U), Unbox, Vector, absA, applyStencil, avgStencil, compute, computeAs, defRowMajor, expA, expandWithin, foldlInner, makeSplitSeedArray, negateA, noPadding, normL2, recipA, sqrtA, sunfoldrExactN, transpose, (!), (!*!), (!+!), (!-!), (!/!), (!><!), (*.), (+.), (-.), (.+), (.-))
import Data.Massiv.Array qualified as A (map)
import GHC.Float (double2Float)
import GHC.Natural (Natural)
import Statistics.Distribution (ContGen (genContVar))
import Statistics.Distribution.Normal (standard)
import System.Random (RandomGen (split))
import System.Random.Stateful (runStateGen)

-- Activation function:
data Activation = Relu | Sigmoid | Tanh | Id deriving (Show)

-- Neural network layer: weights, biases, and activation
data Layer a = Layer !(Matrix U a) !(Vector U a) !Activation
  deriving (Show)

-- \| Batchnorm1d (Vector a) (Vector a) (Vector a) (Vector a)

data NeuralNetworkConfig = NeuralNetworkConfig !Int ![(Int, Activation)]

type NeuralNetwork a = [Layer a]

-- | Weight and bias gradients
data Gradients a = Gradients !(Matrix U a) !(Vector U a)

instance (Unbox e, Floating e) => NumericFloat U e

-- | Lookup activation function by a symbol
getActivation :: Activation -> (Matrix U Float -> Matrix U Float)
getActivation = \case
  Id -> id
  Sigmoid -> \x -> recipA (1.0 +. expA (negateA x))
  Relu -> compute . A.map (max 0)
  Tanh -> \x -> 2 *. recipA (1 +. expA (negateA $ 2 *. x)) .- 1

-- | Lookup activation function derivative by a symbol
getActivation' :: Activation -> Matrix U Float -> (Matrix U Float -> Matrix U Float)
getActivation' Id _ = id
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
  NeuralNetwork Float ->
  -- | Data set
  RunNet s Float ->
  -- | NN computation from forward pass and weights gradients
  ((Matrix U Float, Matrix U Float), [Gradients Float])
pass net run = snd . _pass net $ case run of
  Train x _ -> x
  Infer x -> x
  where
    _pass [] prediction' =
      let -- Gradient of cross-entropy loss
          -- after sigmoid activation.
          loss = case run of
            Train _ target -> prediction' !-! target
            Infer _ -> undefined -- HACK
       in (loss, ((prediction', loss), []))
    _pass (Layer w b sact : layers) inp =
      let Sz2 _ inpC = size inp
          lin = (w !><! inp) !+! (compute $ expandWithin Dim1 (Sz1 inpC) (\v _ -> v) b)
          y = compute $ getActivation sact lin
          (dZ, (predictionAndLoss, gradients)) = _pass layers y
          dY = getActivation' sact lin $ dZ
          dY' = compute dY
          dW = compute $ A.map (/ fromIntegral inpC) $ dY' !><! compute (transpose inp)
          dB = compute $ foldlInner (+) 0 dY
          dX = compute (transpose w) !><! dY'
       in (dX, (predictionAndLoss, Gradients dW dB : gradients))

-- | Gradient descent optimization
optimize ::
  -- | Learning rate
  Float ->
  -- | No of iterations
  Natural ->
  -- | Neural network
  NeuralNetwork Float ->
  -- | Dataset
  RunNet TrainMode Float ->
  -- | Updated neural network, training accuracy + training loss data
  (NeuralNetwork Float, [(Int, (Float, Float))])
optimize lr iterN net runNet@(Train _ tgt) = second ($ []) . flip para iterN $ \case
  Nothing -> (net, id)
  Just (epoch, (net', appendTrainingAccuracyAndLossData)) ->
    ( zipWith f net' dNet,
      appendTrainingAccuracyAndLossData . ((fromIntegral epoch, (inferBinary' predictions `accuracy'` tgt, normL2 loss)) :)
    )
    where
      ((predictions, loss), dNet) = pass net' runNet
      f (Layer w b act) (Gradients dW dB) = Layer (w !-! lr *. dW) (b !-! lr *. dB) act

data AdamParameters = AdamParameters
  { _beta1 :: !Float,
    _beta2 :: !Float,
    _epsilon :: !Float,
    _lr :: !Float
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
  NeuralNetwork Float ->
  -- | Dataset
  RunNet TrainMode Float ->
  (NeuralNetwork Float, [(Int, (Float, Float))])
optimizeAdam p iterN w0 dataSet = (w, trainingAccuracyAndLossData)
  where
    s0 = map zf w0
    v0 = map zf w0
    zf (Layer a b _) = (zerosLike a, zerosLike b)
    zerosLike m = makeArray (ParN 4) (size m) $ \_ -> 0
    ((w, _, _), trainingAccuracyAndLossData) = _adam p iterN (w0, s0, v0) dataSet

_adam ::
  AdamParameters ->
  Natural ->
  (NeuralNetwork Float, [(Matrix U Float, Vector U Float)], [(Matrix U Float, Vector U Float)]) ->
  RunNet TrainMode Float ->
  ((NeuralNetwork Float, [(Matrix U Float, Vector U Float)], [(Matrix U Float, Vector U Float)]), [(Int, (Float, Float))])
_adam
  AdamParameters
    { _lr = lr,
      _beta1 = beta1,
      _beta2 = beta2,
      _epsilon = epsilon
    }
  iterN
  (w0, s0, v0)
  dataSet@(Train _ tgts) = second ($ []) . flip para iterN $ \case
    Nothing -> ((w0, s0, v0), id)
    Just (epoch, ((w, s, v), appendTrainingAccuracyAndLossData)) -> ((wN, sN, vN), appendTrainingAccuracyAndLossData . ((fromIntegral epoch, (inferBinary' predictions `accuracy'` tgts, normL2 loss)) :))
      where
        ((predictions, loss), gradients) = pass w dataSet

        sN = zipWith f2 s gradients
        vN = zipWith f3 v gradients
        wN = zipWith3 f w vN sN

        f ::
          Layer Float ->
          (Matrix U Float, Vector U Float) ->
          (Matrix U Float, Vector U Float) ->
          Layer Float
        f (Layer w_ b_ sf) (vW, vB) (sW, sB) =
          Layer
            (w_ !-! (compute $ lr *. vW !/! ((sqrtA sW) .+ epsilon)))
            (b_ !-! (compute $ lr *. vB !/! ((sqrtA sB) .+ epsilon)))
            sf

        f2 ::
          (Matrix U Float, Vector U Float) ->
          Gradients Float ->
          (Matrix U Float, Vector U Float)
        f2 (sW, sB) (Gradients dW dB) =
          ( beta2 *. sW !+! (1 - beta2) *. (dW !*! dW),
            beta2 *. sB !+! (1 - beta2) *. (dB !*! dB)
          )

        f3 ::
          (Matrix U Float, Vector U Float) ->
          Gradients Float ->
          (Matrix U Float, Vector U Float)
        f3 (vW, vB) (Gradients dW dB) =
          ( beta1 *. vW !+! (1 - beta1) *. dW,
            beta1 *. vB !+! (1 - beta1) *. dB
          )

-- | Generate a neural network with random weights
genNetwork :: (RandomGen g) => g -> NeuralNetworkConfig -> (NeuralNetwork Float, g)
genNetwork g (NeuralNetworkConfig nStart l) =
  flip para ((nStart, undefined) :| l) $ \case
    NonEmptyF (nIn, _) mr -> case mr of
      Nothing -> ([], g)
      Just ((nOut, activation) :| _, (layers, split -> (split -> (g1, g2), g3))) ->
        let w = computeAs U $ makeSplitSeedArray defRowMajor g1 split (ParN 4) (Sz2 nOut nIn) $ \_ _ g' -> first double2Float (runStateGen g' $ genContVar standard)
            b = compute $ sunfoldrExactN (Sz1 nOut) (\g' -> first double2Float (runStateGen g' $ genContVar standard)) g2
         in (Layer w b activation : layers, g3)

-- | Perform a binary classification
inferBinary' :: Matrix U Float -> Matrix U Float
inferBinary' predictions = compute $ A.map (\a -> if a < 0.5 then 0 else 1) predictions

-- | Perform a binary classification
inferBinary ::
  NeuralNetwork Float -> Matrix U Float -> Matrix U Float
inferBinary net dta = inferBinary' . fst . fst . pass net $ Infer dta

-- | Binary classification accuracy in percent
accuracy' ::
  -- | Predicitons
  Matrix U Float ->
  -- | Targets
  Matrix U Float ->
  Float
accuracy' preditions tgts = 100 * (1 - (avg ! (Ix2 0 0)))
  where
    a = absA $ tgts !-! preditions
    avg = compute @U $ applyStencil noPadding (avgStencil $ size a) a

-- | Binary classification accuracy in percent
accuracy ::
  -- | Neural network
  [Layer Float] ->
  -- | Dataset
  RunNet TrainMode Float ->
  Float
accuracy net (Train dta tgts) = accuracy' (net `inferBinary` dta) tgts
