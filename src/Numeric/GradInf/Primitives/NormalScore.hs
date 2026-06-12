module Numeric.GradInf.Primitives.NormalScore where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.Normal

-- Class definition

class (Monad m) => NormalScore m r | m r -> r where
    normalScore :: (r, r) -> m r

-- Core semantics

instance NormalScore Sampler Double where
    normalScore = normal

-- Supporting inference

instance (Normal Sampler d) => NormalScore Sampler (Expected d d) where
    normalScore = normal

-- Support coupling + partial eval

normalPdf :: (DeterministicPrimitives d i b mat, Normal Sampler d) => (d, d) -> d -> d
normalPdf (mu, sigma) x = do
    1 / (sigma * sqrt (2 * pi)) * exp (-0.5 * ((x - mu) / sigma) ^ 2)

instance
    (MonadFactor d m0, DeterministicPrimitives d i b mat, Normal Sampler d) =>
    NormalScore (WrapperSamplerT m0) (Coupled d)
    where
    normalScore ((Coupled (muA, muB)), (Coupled (sigmaA, sigmaB))) = do
        xA <- primal (normal (muA, sigmaA))
        let pdfA = normalPdf (muA, sigmaA) xA
        let pdfB = normalPdf (muB, sigmaB) xA
        xB <- residual (score (pdfB / pdfA) >> return xA)
        return (Coupled (xA, xB))
