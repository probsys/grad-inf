module Numeric.GradInf.Primitives.Uniform where

import Control.Monad.Bayes.Class qualified as Bayes (random)
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

class (Monad m) => Uniform m r where
    uniform :: m r

-- Core semantics

instance Uniform Sampler Double where
    uniform = Bayes.random

-- Supporting inference

instance (Uniform m r) => Uniform (StateT s m) r where
    uniform = lift uniform

instance (Uniform Sampler d) => Uniform Sampler (Base d d) where
    uniform = fmap Base uniform

instance (DeterministicPrimitives d i b mat) => Uniform Sampler (Expected d d) where
    uniform = fmap fromDouble uniform

instance (Uniform m d) => Uniform (WeightedT d m) d where
    uniform = lift uniform

instance (Uniform m d) => Uniform (PopulationT d m) d where
    uniform = lift uniform

instance Uniform Sampler (Forward Double) where
    uniform = fmap pureForward (uniform)

instance (Uniform Sampler d) => Uniform Sampler (AD s d) where
    uniform = fmap AD (uniform)

instance (Reifies s W) => Uniform Sampler (BVar s Double) where
    -- Intentionally used fmap instead of do notation because this gets the
    -- \*seeded* semantics to match the usual flip too, which is useful
    -- for fixed-seed testing.
    uniform = fmap pureBVar uniform

-- Support coupling + partial eval

instance
    (Monad m0, Uniform Sampler d, Show d) =>
    Uniform (WrapperSamplerT m0) (Coupled d)
    where
    uniform = do
        x <- primal uniform
        return (Coupled (x, x))
