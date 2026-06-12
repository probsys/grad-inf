module Numeric.GradInf.Primitives.FlipEnum where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT(..))
import Control.Monad.Trans (lift)
import Prelude hiding (flip)
import Data.Functor.Product

import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.AD.AD
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.Inference.Sequential

import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.Flip

-- Class definition

class (Monad m) => FlipEnum m r b where
    flipEnum :: r -> m b

-- Core semantics

instance FlipEnum Sampler Double Bool where
    flipEnum = flip

instance FlipEnum Sampler (AD s (Forward Double)) (AD s (Forward Bool)) where
    flipEnum = fmap AD . fmap pureForward . flipEnum . extractForwardNum . runAD

-- Supporting inference

instance (FlipEnum m r b) => FlipEnum (StateT s m) r b where
    flipEnum = lift . flipEnum

instance
    (Flip Sampler (Expected d d) (Expected d b)) =>
    FlipEnum Sampler (Expected d d) (Expected d b)
    where
    flipEnum = flip

instance
    (Flip (Sequential susp) (Expected d d) tt) =>
    FlipEnum (Sequential susp) (Expected d d) tt
    where
    flipEnum = flip

instance (FlipEnum m d b) => FlipEnum (WeightedT d m) d b where
    flipEnum = lift . flipEnum

instance (DeterministicPrimitives d i b mat, Monad m) => FlipEnum (PopulationT d m) d b where
    flipEnum p = do
        populationT (return [(fromBool True, p), (fromBool False, 1 - p)])

instance (DeterministicPrimitives d i b mat, Monad m) => FlipEnum (ResamplerT resampler d m) d b where
    flipEnum = ResamplerT . (populationT . lift . runPopulationT) . flipEnum

instance (DeterministicPrimitives d i b mat, Monad m) => FlipEnum (LoggedResamplerT resampler logA d m) d b where
    flipEnum = LoggedResamplerT . (populationT . lift . runPopulationT) . flipEnum

instance
    (Monad m, DeterministicPrimitives d i b mat) =>
    FlipEnum (WeightedPopulationT d m) d b
    where
    flipEnum = WeightedPopulationT . flipEnum

instance (FlipEnum m1 d b, FlipEnum m2 d b) => FlipEnum (Product m1 m2) d b where
    flipEnum p = Pair (flipEnum p) (flipEnum p)
