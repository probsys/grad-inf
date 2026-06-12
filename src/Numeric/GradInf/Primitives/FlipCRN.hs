{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.FlipCRN where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)
import Prelude hiding (flip)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.Flip
import Numeric.GradInf.Primitives.FlipEnum

-- Class definition

class (Monad m) => FlipCRN m r b where
    flipCRN :: r -> m b

-- Core semantics

instance FlipCRN Sampler Double Bool where
    flipCRN = flip

-- Supporting inference

instance
    (Flip Sampler (Expected d d) (Expected d b)) =>
    FlipCRN Sampler (Expected d d) (Expected d b)
    where
    flipCRN = flip

-- Support coupling + partial eval

-- TODO: get the more natural min/max code to also play nicely with AD here
instance
    (Monad m0, DeterministicPrimitives d i b mat, Flip Sampler d b, FlipEnum m0 d b) =>
    FlipCRN (WrapperSamplerT m0) (Coupled d) (Coupled b)
    where
    flipCRN (Coupled (pA, pB)) = do
        xA <- primal (flip pA)
        xB <-
            residual
                (flipEnum (if xA then (if (isGreater pB pA) :: b then 1 else (pB / pA)) else (if (isGreater pB pA) :: b then ((pB - pA) / (1 - pA)) else 0)))
        return (Coupled (xA, xB))
