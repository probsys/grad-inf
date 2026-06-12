{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.FlipScore where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)
import Prelude hiding (flip)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.Flip

-- Class definition

class (Monad m) => FlipScore m r b where
    flipScore :: r -> m b

-- Core semantics

instance FlipScore Sampler Double Bool where
    flipScore = flip

-- Supporting inference

instance
    (Flip Sampler (Expected d d) (Expected d b)) =>
    FlipScore Sampler (Expected d d) (Expected d b)
    where
    flipScore = flip

-- Support coupling + partial eval

instance
    (MonadFactor d m, DeterministicPrimitives d i b mat, Flip Sampler d b, Show d) =>
    FlipScore (WrapperSamplerT m) (Coupled d) (Coupled b)
    where
    flipScore (Coupled (pA, pB)) = do
        xA <- primal (flip pA)
        if (pA == 0 || pA == 1) && pB /= pA
            then
                error
                    ( "flipScore: only correct when pA == 0 or pA == 1 if pB == pA (pA = "
                        ++ show pA
                        ++ ", pB = "
                        ++ show pB
                        ++ ")"
                    )
            else
                return ()
        xB <-
            residual (score (if xA then pB / pA else (1 - pB) / (1 - pA)) >> return xA)
        return (Coupled (xA, xB))
