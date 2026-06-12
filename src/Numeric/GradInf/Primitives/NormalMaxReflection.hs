{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.NormalMaxReflection where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)
import Prelude hiding (flip)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.FlipEnum
import Numeric.GradInf.Primitives.Normal

-- Class definition

class (Monad m) => NormalMaxReflection m r | m r -> r where
    normalMaxReflection :: (r, r) -> m r

-- Core semantics

instance NormalMaxReflection Sampler Double where
    normalMaxReflection = normal

-- Supporting inference

instance (Normal Sampler (Expected d d)) => NormalMaxReflection Sampler (Expected d d) where
    normalMaxReflection = normal

-- Support coupling + partial eval

instance
    ( Monad m0
    , DeterministicPrimitives d i b mat
    , Normal Sampler d
    , FlipEnum m0 d b
    , Show d
    ) =>
    NormalMaxReflection (WrapperSamplerT m0) (Coupled d)
    where
    normalMaxReflection ((Coupled (muA, muB)), (Coupled (sigmaA, sigmaB))) = do
        xA <- primal (normal (muA, sigmaA))
        if (sigmaA /= sigmaB)
            then
                error ("normalMaxReflection: only supports sigmaA == sigmaB")
            else
                return ()
        xB <-
            residual
                ( do
                    let alpha = min 0 (-1 / 2 * ((xA - muB) ** 2 - (xA - muA) ** 2) / (sigmaA ** 2))
                    b <- flipEnum (exp alpha)
                    let xB = if b :: b then xA else muA + muB - xA
                    return xB
                )
        if ((signum (xB - xA)) /= (signum (muB - muA))) && (xB /= xA)
            then
                error
                    ( "normalMaxReflection: expected monotone coupling"
                        ++ "(xA, xB) = "
                        ++ show (xA, xB)
                        ++ " (muA, muB) = "
                        ++ show (muA, muB)
                    )
            else
                return ()
        return (Coupled (xA, xB))
