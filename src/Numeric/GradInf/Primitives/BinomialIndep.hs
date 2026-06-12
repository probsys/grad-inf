module Numeric.GradInf.Primitives.BinomialIndep where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.Binomial

-- Class definition

class (Monad m) => BinomialIndep m r i where
    binomialIndep :: (i, r) -> m i

-- Core semantics

instance BinomialIndep Sampler Double Int where
    binomialIndep = binomial

-- Supporting inference

instance
    (Binomial Sampler (Expected d d) (Expected d i)) =>
    BinomialIndep Sampler (Expected d d) (Expected d i)
    where
    binomialIndep = binomial

-- Support coupling + partial eval

instance
    (Monad m0, Binomial Sampler d i, Binomial m0 d i) =>
    BinomialIndep (WrapperSamplerT m0) (Coupled d) (Coupled i)
    where
    binomialIndep ((Coupled (nA, nB), Coupled (pA, pB))) = do
        xA <- primal (binomial (nA, pA))
        xB <- residual (binomial (nB, pB))
        return (Coupled (xA, xB))
