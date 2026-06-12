module Numeric.GradInf.Primitives.NormalCRN where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.Normal

-- Class definition

class (Monad m) => NormalCRN m r | m r -> r where
    normalCRN :: (r, r) -> m r

-- Core semantics

instance NormalCRN Sampler Double where
    normalCRN = normal

-- Supporting inference

instance (Normal Sampler d) => NormalCRN Sampler (Expected d d) where
    normalCRN = normal

-- Support coupling + partial eval

instance
    (Monad m0, DeterministicPrimitives d i b mat, Normal Sampler d) =>
    NormalCRN (WrapperSamplerT m0) (Coupled d)
    where
    normalCRN ((Coupled (muA, muB)), (Coupled (sigmaA, sigmaB))) = do
        omega <- primal (normal (0, 1))
        let xA = muA + sigmaA * omega
        let xB = muB + sigmaB * omega
        return (Coupled (xA, xB))
