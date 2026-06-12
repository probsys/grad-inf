module Numeric.GradInf.Primitives.FlipIndep where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)
import Prelude hiding (flip)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.Flip

-- Class definition

class (Monad m) => FlipIndep m r b where
    flipIndep :: r -> m b

-- Core semantics

instance FlipIndep Sampler Double Bool where
    flipIndep = flip

-- Supporting inference

instance
    (Flip Sampler (Expected d d) (Expected d b)) =>
    FlipIndep Sampler (Expected d d) (Expected d b)
    where
    flipIndep = flip

-- Support coupling + partial eval

instance
    (Monad m0, Flip Sampler d b, Flip m0 d b) =>
    FlipIndep (WrapperSamplerT m0) (Coupled d) (Coupled b)
    where
    flipIndep (Coupled (pA, pB)) = do
        xA <- primal (flip pA)
        xB <- residual (flip pB)
        return (Coupled (xA, xB))
