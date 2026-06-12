{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Numeric.GradInf (
       module Numeric.GradInf,
       module Numeric.GradInf.Inference,
       Coupled (..)
)
where

import Control.Monad.Bayes.Sampler.Lazy (Sampler, randomTree, runSampler)
import System.Random (getStdGen, newStdGen)

import Numeric.GradInf.Coupling
import Numeric.GradInf.PartialProbabilityEval
import Numeric.GradInf.Primitives.FunctorRep
import Numeric.GradInf.Inference
import Numeric.GradInf.AD.AD
import Numeric.GradInf.AD.Backprop (BVar, W, Reifies)
import Numeric.AD qualified as AD
import Numeric.Backprop qualified as BP
import Numeric.LinearAlgebra (Matrix, cmap)
import Data.Kind (Constraint)

-- A replacement for a missing method in monad-bayes 1.3.0.4.
sampler :: Sampler a -> IO a
sampler m = newStdGen *> (runSampler m . randomTree <$> getStdGen)

type InputFunc (mInf :: * -> *) (mBase :: * -> * -> *) d f
       = f (Coupled (mBase d d)) -> WrapperSamplerT mInf (Coupled (mBase d d))

gradInfFDGeneric :: forall alg mBase d i b f. (InferenceAlg alg mBase d i b, Functor f) => InputFunc (MInf alg d i b) mBase d f -> alg d i b -> f (Coupled d) -> Sampler d
gradInfFDGeneric factoredAndCoupled alg thetaC = do
    let target = do
            Coupled (xA, xB) <- factoredAndCoupled (fmap (\(Coupled (xL, xR)) -> Coupled (toBase xL, toBase xR)) thetaC)
            return (xA, xB)
    let fdNotErased = fmap fromBase $ keepPrimalInference (runInference alg) target
    let fd = do
           mu <- getWrapperSamplerT fdNotErased
           mu
    fd

gradInfFD :: forall alg mBase b f. (InferenceAlg alg mBase Double Int b, Functor f) => InputFunc (MInf alg Double Int b) mBase Double f -> alg Double Int b -> f (Coupled Double) -> Sampler Double
gradInfFD = gradInfFDGeneric

gradInfAD ::  forall f alg mBase. (Traversable f, Applicative f, forall s d i b. (d ~ AD s (Forward Double), i ~ AD s (Forward Int), b ~ AD s (Forward Bool)) => InferenceAlg alg mBase d i b) =>
                     (forall s d i b. (d ~ AD s (Forward Double), i ~ AD s (Forward Int), b ~ AD s (Forward Bool)) => InputFunc (MInf alg d i b) mBase d f) ->
                     (forall s d i b. (d ~ AD s (Forward Double), i ~ AD s (Forward Int), b ~ AD s (Forward Bool)) => alg d i b) ->
                     f Double ->
                     Sampler (f Double)
gradInfAD factoredAndCoupled alg theta = do
       let gradInfTarget :: forall s. f (AD s (Forward Double)) -> Sampler (AD s (Forward Double))
           gradInfTarget eps = do
              let inputCoupled :: f (Coupled (AD s (Forward Double))) = (liftA2 (\theta eps -> Coupled (AD.auto theta, AD.auto theta + eps)) theta eps)
              gradInfFDGeneric (factoredAndCoupled @s) (alg @s) inputCoupled
       seed <- getSeed
       let (thetaWithIndices, n) = withIndices theta
       let diffAtIndex i = AD.diff (seeded seed . gradInfTarget . (\eps -> fmap (\(_, j) -> if i == j then eps else 0) thetaWithIndices)) (0 :: Double)
       return (fmap (\(_, i) -> diffAtIndex i) thetaWithIndices)

-- Below, we reimplement the above infrastructure for matrix-input and reverse-mode AD. --

-- Like InputFunc but for matrix-valued parameters: the functor holds matrices
-- while the model still returns a scalar d.
type InputFuncMatrix (mInf :: * -> *) (mBase :: * -> * -> *) d mat f
       = f (Coupled (mBase mat mat)) -> WrapperSamplerT mInf (Coupled (mBase d d))

-- Like gradInfFDGeneric but for matrix-valued parameters: params live in mat,
-- model output is still scalar d. Uses InferenceBase mBase to lift matrix params.
gradInfFDGenericMatrix :: forall alg mBase d i b mat f.
    (InferenceAlg alg mBase d i b, Functor f) =>
    InputFuncMatrix (MInf alg d i b) mBase d mat f ->
    alg d i b ->
    f (Coupled mat) ->
    Sampler d
gradInfFDGenericMatrix factoredAndCoupled alg thetaC = do
    let target = do
            Coupled (xA, xB) <- factoredAndCoupled (fmap (\(Coupled (xL, xR)) -> Coupled (toBase xL, toBase xR)) thetaC)
            return (xA, xB)
    let fdNotErased = fmap fromBase $ keepPrimalInference (runInference alg) target
    let fd = do
           mu <- getWrapperSamplerT fdNotErased
           mu
    fd

gradInfReverseAD :: forall f alg mBase.
    ( Traversable f
    , Applicative f
    , BP.Backprop (f (Matrix Double))
    , forall s. Reifies s W => InferenceAlg alg mBase (BVar s Double) (BVar s Int) (BVar s Bool)
    ) =>
    (forall s. Reifies s W => InputFuncMatrix (MInf alg (BVar s Double) (BVar s Int) (BVar s Bool)) mBase (BVar s Double) (BVar s (Matrix Double)) f) ->
    (forall s. Reifies s W => alg (BVar s Double) (BVar s Int) (BVar s Bool)) ->
    f (Matrix Double) ->
    Sampler (f (Matrix Double))
gradInfReverseAD factoredAndCoupled alg theta = do
    let gradInfTarget :: forall s. Reifies s W => f (BVar s (Matrix Double)) -> Sampler (BVar s Double)
        gradInfTarget eps = do
            let inputCoupled = liftA2 (\t e -> Coupled (BP.auto t, BP.auto t + e)) theta eps
            gradInfFDGenericMatrix (factoredAndCoupled @s) (alg @s) inputCoupled
    seed <- getSeed
    -- Differentiate at eps = 0 (zero-matrix perturbation), mirroring AD.diff ... (0 :: Double)
    return (BP.gradBP (seeded seed . gradInfTarget . BP.sequenceVar) (fmap (cmap (const 0)) theta))