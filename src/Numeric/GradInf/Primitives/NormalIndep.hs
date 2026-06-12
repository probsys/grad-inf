module Numeric.GradInf.Primitives.NormalIndep where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.Normal

-- Class definition

class (Monad m) => NormalIndep m r | m r -> r where
    normalIndep :: (r, r) -> m r

-- Core semantics

instance NormalIndep Sampler Double where
    normalIndep = normal

-- Supporting inference

instance (Normal Sampler d) => NormalIndep Sampler (Expected d d) where
    normalIndep = normal

-- Support coupling + partial eval

instance
    (MonadFactor d m0, DeterministicPrimitives d i b mat, Normal Sampler d) =>
    NormalIndep (WrapperSamplerT m0) (Coupled d)
    where
    normalIndep ((Coupled (muA, muB)), (Coupled (sigmaA, sigmaB))) = do
        xA <- primal (normal (muA, sigmaA))
        xB <- primal (normal (muB, sigmaB))
        return (Coupled (xA, xB))
