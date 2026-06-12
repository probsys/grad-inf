{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.GradInf.Inference where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT (..), runStateT)
import Data.Vector (Vector)
import Data.Vector.Generic qualified as Vector
import Data.Functor.Product
import Data.Kind (Type)
import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.FunctorRep
import Numeric.GradInf.PartialProbabilityEval
import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Base
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.Inference.Sequential
import Numeric.GradInf.Inference.Expected

keepPrimalInference :: (m0 (d, d) -> Sampler d) -> WrapperSamplerT m0 (d, d) -> WrapperSamplerT Sampler d
keepPrimalInference innerInference = WrapperSamplerT . fmap innerInference . getWrapperSamplerT

class InferenceBase mBase where
  toBase :: forall t. t -> mBase t t
  fromBase :: forall t. mBase t t -> t

instance InferenceBase Base where
  toBase = Base
  fromBase (Base x) = x

instance InferenceBase Expected where
 toBase = Expected
 fromBase (Expected x) = x

class ( Num (mBase d d), Monad (MInf alg d i b), InferenceBase mBase) => InferenceAlg (alg :: * -> * -> * -> *) mBase d i b | alg -> mBase where
  type MInf  alg d i b :: * -> *
  runInference :: alg d i b -> MInf alg d i b (mBase d d, mBase d d) -> Sampler (mBase d d)


-- PopulationInference

data PopulationInference (mBase :: * -> * -> *) d i b = PopulationInference
type PopulationInferenceAlg d i b = PopulationInference Base d i b

populationInferenceAlg :: PopulationInferenceAlg d i b
populationInferenceAlg = PopulationInference

instance (Num (mBase d d), DeterministicPrimitives (mBase d d) (mBase d i) (mBase d b) (mBase mat mat), InferenceBase mBase) => InferenceAlg (PopulationInference mBase) mBase d i b where
    type MInf (PopulationInference mBase) d i b = PopulationT (mBase d d) Sampler
    runInference _ inferenceProgram = do
        pop <- runPopulationT inferenceProgram
        let (xs, ws) = unzip pop
        return (sum (zipWith (\(xA, xB) w -> w * (xB - xA)) xs ws))

-- ResamplerInference

data ResamplerInference (mBase :: * -> * -> *) resampler d i b = ResamplerInference resampler
type ResamplerInferenceAlg resampler d i b = ResamplerInference Base resampler d i b

resamplerInferenceAlg :: resampler -> ResamplerInferenceAlg resampler d i b
resamplerInferenceAlg = ResamplerInference

instance (Num (mBase d d), DeterministicPrimitives (mBase d d) (mBase d i) (mBase d b) (mBase mat mat), InferenceBase mBase) =>
    InferenceAlg (ResamplerInference mBase resampler) mBase d i b where
    type MInf (ResamplerInference mBase resampler) d i b = ResamplerT resampler (mBase d d) Sampler
    runInference (ResamplerInference resampler) inferenceProgram = do
        let (ResamplerT resamplerProgram) = inferenceProgram
        let popProgram = populationT (fmap fst (runStateT (runPopulationT resamplerProgram) (resampler, 0)))
        runInference (PopulationInference @mBase @d) popProgram

-- WeightedResamplerInference

data WeightedResamplerInference (mBase :: * -> * -> *) resampler d i b = WeightedResamplerInference resampler
type WeightedResamplerInferenceAlg resampler d i b = WeightedResamplerInference Base resampler d i b

weightedResamplerInferenceAlg :: resampler -> WeightedResamplerInferenceAlg resampler d i b
weightedResamplerInferenceAlg = WeightedResamplerInference

instance (Num (mBase d d), DeterministicPrimitives (mBase d d) (mBase d i) (mBase d b) (mBase mat mat), InferenceBase mBase) =>
    InferenceAlg (WeightedResamplerInference mBase resampler) mBase d i b where
    type MInf (WeightedResamplerInference mBase resampler) d i b = WeightedResamplerT resampler (mBase d d) Sampler
    runInference (WeightedResamplerInference resampler) inferenceProgram = do
        let (WeightedResamplerT (WeightedPopulationT weightedPop)) = inferenceProgram
        pop <-
            fmap fst $
                runStateT
                    (runWeightedPopulationT (WeightedPopulationT weightedPop))
                    (resampler, 0)
        let n = length pop
        if n > 5
            then error $ "WeightedResamplerInference runInference: population length " ++ show n ++ " exceeds 5"
            else return (sum (map (\(((xA, xB), w1), w2) -> w1 * w2 * (xB - xA)) pop))

-- VariableEliminationInference

data VariableEliminationInference (mBase :: * -> * -> *) d i b = VariableEliminationInference
type VariableEliminationInferenceAlg d i b = VariableEliminationInference Base d i b

variableEliminationInferenceAlg :: VariableEliminationInferenceAlg d i b
variableEliminationInferenceAlg = VariableEliminationInference

instance (Num (mBase d d), DeterministicPrimitives (mBase d d) (mBase d i) (mBase d b) (mBase mat mat), InferenceBase mBase) =>
    InferenceAlg (VariableEliminationInference mBase) mBase d i b where
    type MInf (VariableEliminationInference mBase) d i b = ResamplerT KeepDistinctResampler (mBase d d) Sampler
    runInference _ inferenceProgram = runInference (ResamplerInference @mBase @_ @d KeepDistinctResampler) inferenceProgram

-- StratifiedImportanceResamplingInference

data StratifiedImportanceResamplingInference (mBase :: * -> * -> *) d i b = StratifiedImportanceResamplingInference Int

type StratifiedImportanceResamplingInferenceAlg d i b = StratifiedImportanceResamplingInference Base d i b

stratifiedImportanceResamplingInferenceAlg :: Int -> StratifiedImportanceResamplingInferenceAlg d i b
stratifiedImportanceResamplingInferenceAlg = StratifiedImportanceResamplingInference

instance (Num (mBase d d), DeterministicPrimitives (mBase d d) (mBase d i) (mBase d b) (mBase mat mat), InferenceBase mBase) =>
    InferenceAlg (StratifiedImportanceResamplingInference mBase) mBase d i b where
    type MInf (StratifiedImportanceResamplingInference mBase) d i b = ResamplerT (KeepLargeWeightsResampler MultinomialResampler) (mBase d d) Sampler
    runInference (StratifiedImportanceResamplingInference numParticles) inferenceProgram =
        runInference (ResamplerInference @_ @_ @d (KeepLargeWeightsResampler (MultinomialResampler numParticles) 0.5)) inferenceProgram

-- TwistedSMCInference

class Assoc t where
  type Obj t (d :: Type) (i :: Type) (b :: Type) :: Type

data Coupling

instance Assoc Coupling where
  type Obj Coupling d i b = Coupled d

data CRN

instance Assoc CRN where
  type Obj CRN d i b = ([Coupled i], Coupled d, [Coupled d])

data BernoulliLayer

instance Assoc BernoulliLayer where
  type Obj BernoulliLayer d i b = [Coupled b]

data TwistedSMCInference mBase t d i b =
    TwistedSMCInference Int (Int -> Obj t (mBase d d) (mBase d i) (mBase d b) -> mBase d d)

type TwistedSMCInferenceAlg t d i b = TwistedSMCInference Base t d i b

twistedSMCInferenceAlg :: forall t d i b. (Int -> Obj t (Base d d) (Base d i) (Base d b) -> Base d d) -> Int -> TwistedSMCInferenceAlg t d i b
twistedSMCInferenceAlg twistFunc numParticles = TwistedSMCInference numParticles twistFunc

instance (DeterministicPrimitives (mBase d d) (mBase d i) (mBase d b) (mBase mat mat), InferenceBase mBase) =>
    InferenceAlg (TwistedSMCInference mBase t) mBase d i b where
    type MInf (TwistedSMCInference mBase t) d i b = (ResamplerT (KeepLargeWeightsResampler (TwistedResampler (TwoSidedResampler MultinomialResampler MultinomialResampler) (mBase d d) (Obj t (mBase d d) (mBase d i) (mBase d b)))) (mBase d d) Sampler)
    runInference (TwistedSMCInference numParticles twistFunc) inferenceProgram = do
        let resampler = KeepLargeWeightsResampler (TwistedResampler (TwoSidedResampler (MultinomialResampler numParticles) (MultinomialResampler numParticles)) twistFunc) 0.5
        runInference (ResamplerInference @_ @_ @d resampler) inferenceProgram

-- MeanFieldInference

data MeanFieldInference (mBase :: * -> * -> *) t d i b = MeanFieldInference
type MeanFieldInferenceAlg t d i b = MeanFieldInference Expected t d i b

meanFieldInferenceAlg :: MeanFieldInferenceAlg t d i b
meanFieldInferenceAlg = MeanFieldInference

instance (Num (mBase d d), DeterministicPrimitives (mBase d d) (mBase d i) (mBase d b) (mBase mat mat), FunctorRep (Obj t (mBase d d) (mBase d i) (mBase d b)) (mBase d d) susp,
      Show (Obj t (mBase d d) (mBase d i) (mBase d b)), InferenceBase mBase) =>
    InferenceAlg (MeanFieldInference mBase t)  mBase d i b where
    type MInf (MeanFieldInference mBase t) d i b = (Product (Sequential (Obj t (mBase d d) (mBase d i) (mBase d b))) (ResamplerT (KeepLargeWeightsResampler (TwistedResampler (TwoSidedResampler MultinomialResampler MultinomialResampler) (mBase d d) (Obj t (mBase d d) (mBase d i) (mBase d b)))) (mBase d d) Sampler))
    runInference _ inferenceProgram = do
        let Pair m10 m2 = inferenceProgram
        let m1 = fmap (\(x, y) -> y - x) m10
        let SequentialNonEmpty getOriginalValue getFirstSuspended sequentialFuncs curFunc = m1
        let suspendedValues = Vector.scanl (\susp f -> f susp) (getFirstSuspended ()) sequentialFuncs

        return (curFunc (Vector.last suspendedValues))

-- ScoreInference

data ScoreInference (mBase :: * -> * -> *) d i b = ScoreInference Double

-- Convenience alias fixing mBase = Base for score-function estimators.
type ScoreInferenceAlg d i b = ScoreInference Base d i b

scoreInferenceAlg :: Double -> ScoreInferenceAlg d i b
scoreInferenceAlg baseline = ScoreInference baseline

instance (Num (mBase d d), DeterministicPrimitives (mBase d d) (mBase d i) (mBase d b) (mBase mat mat), InferenceBase mBase) =>
    InferenceAlg (ScoreInference mBase) mBase d i b where
    type MInf (ScoreInference mBase) d i b = PopulationT (mBase d d) Sampler
    runInference (ScoreInference baseline) inferenceProgram = do
        pop <- runPopulationT inferenceProgram
        let (xs, ws) = unzip pop
        return (sum (zipWith (\(xA, xB) w -> (xB - fromDouble baseline) * (w - 1) + (xB - xA)) xs ws))

-- StratifiedImportanceResamplingUniformInference

data StratifiedImportanceResamplingUniformInference (mBase :: * -> * -> *) d i b =
    StratifiedImportanceResamplingUniformInference Int

stratifiedImportanceResamplingUniformInferenceAlg ::
    Int -> StratifiedImportanceResamplingUniformInference Base d i b
stratifiedImportanceResamplingUniformInferenceAlg = StratifiedImportanceResamplingUniformInference

instance
    ( Num (mBase d d)
    , FromDouble (mBase d d)
    , DeterministicPrimitives (mBase d d) (mBase d i) (mBase d b) (mBase mat mat)
    , InferenceBase mBase
    ) =>
    InferenceAlg (StratifiedImportanceResamplingUniformInference mBase) mBase d i b
    where
    type MInf (StratifiedImportanceResamplingUniformInference mBase) d i b = WeightedResamplerT (KeepLargeWeightsResampler (ComposeWeightedResampler (PropagateSmallWeightsWeightedResampler (mBase d d)) (RunInnerWeightedResampler MultinomialResampler (mBase d d)))) (mBase d d) Sampler

    runInference (StratifiedImportanceResamplingUniformInference numParticles) inferenceProgram =
        runInference (WeightedResamplerInference @mBase @_ @d (KeepLargeWeightsResampler (ComposeWeightedResampler (PropagateSmallWeightsWeightedResampler (fromDouble 1e-4)) (RunInnerWeightedResampler (MultinomialResampler numParticles))) 0.5)) inferenceProgram