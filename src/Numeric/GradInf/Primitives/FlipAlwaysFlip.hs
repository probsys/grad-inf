{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.FlipAlwaysFlip where

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

class (Monad m) => FlipAlwaysFlip m r b where
    flipAlwaysFlip :: r -> m b

-- Core semantics

instance FlipAlwaysFlip Sampler Double Bool where
    flipAlwaysFlip = flip

-- Supporting inference

instance
    (Flip Sampler (Expected d d) (Expected d b)) =>
    FlipAlwaysFlip Sampler (Expected d d) (Expected d b)
    where
    flipAlwaysFlip = flip

-- Support coupling + partial eval

instance
    (Monad m0, DeterministicPrimitives d i b mat, Flip Sampler d b, FlipEnum m0 d b) =>
    FlipAlwaysFlip (WrapperSamplerT m0) (Coupled d) (Coupled b)
    where
    flipAlwaysFlip (Coupled (pA, pB)) = do
        xA <- primal (flip pA)
        let w = if xA then pA - pB else pB - pA
        b <- residual (flipEnum w)
        let xB = if b :: b then (if xA then (fromBool False) else (fromBool True)) else xA
        return (Coupled (xA, xB))
