module Numeric.GradInf.Primitives.Normal where

import Control.Monad.Bayes.Class qualified as Bayes (normal)
import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.AD.AD
import Numeric.GradInf.AD.Backprop
import Numeric.GradInf.Inference.Base
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC

import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Class definition

class (Monad m) => Normal m r | m r -> r where
    normal :: (r, r) -> m r

-- Core semantics

instance Normal Sampler Double where
    normal (mu, sigma) = Bayes.normal mu sigma

-- Supporting inference

instance (Normal Sampler d) => Normal Sampler (Base d d) where
    normal (Base mu, Base sigma) = fmap Base (normal (mu, sigma))

instance (Normal m r) => Normal (StateT s m) r where
    normal = lift . normal

instance (Normal Sampler d) => Normal Sampler (Expected d d) where
    normal (Expected mu, Expected sigma) = fmap Expected (normal (mu, sigma))

instance (Normal m d) => Normal (WeightedT d m) d where
    normal = lift . normal

instance (Normal m d) => Normal (PopulationT d m) d where
    normal = lift . normal

instance Normal Sampler (Forward Double) where
    normal (mu, sigma) = fmap pureForward (normal (extractForwardNum mu, extractForwardNum sigma))

instance (Normal Sampler d) => Normal Sampler (AD s d) where
    normal (mu, sigma) = fmap AD (normal (runAD mu, runAD sigma))

instance (Reifies s W) => Normal Sampler (BVar s Double) where
    normal (mu, sigma) = fmap (\seed -> mapBVar2 (\m s -> seeded seed (normal (m, s))) mu sigma) getSeed

-- Support coupling + partial eval

instance
    (Monad m0, DeterministicPrimitives d i b mat, Normal Sampler d) =>
    Normal (WrapperSamplerT m0) (Coupled d)
    where
    normal (Coupled (muA, muB), Coupled (sigmaA, sigmaB)) = do
        if (muA /= muB) || (sigmaA /= sigmaB)
            then
                error ("normal: only supports muA == muB and sigmaA == sigmaB")
            else
                return ()
        x <- primal (normal (muA, sigmaA))
        return (Coupled (x, x))
