module Numeric.GradInf.Primitives.CategoricalEnum where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Functor.Product

import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.Inference.Sequential

import Numeric.GradInf.Primitives.Categorical
import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Class definition

class (Monad m) => CategoricalEnum m r tt where
    categoricalEnum :: ([r], [tt]) -> m tt

-- Core semantics

instance CategoricalEnum Sampler Double tt where
    categoricalEnum = categorical

-- Supporting inference

instance (CategoricalEnum m r tt) => CategoricalEnum (StateT s m) r tt where
    categoricalEnum = lift . categoricalEnum

instance
    (Categorical Sampler (Expected d d) tt) =>
    CategoricalEnum Sampler (Expected d d) tt
    where
    categoricalEnum = categorical

instance
    (Categorical (Sequential susp) (Expected d d) tt) =>
    CategoricalEnum (Sequential susp) (Expected d d) tt
    where
    categoricalEnum = categorical

instance (CategoricalEnum m d i) => CategoricalEnum (WeightedT d m) d i where
    categoricalEnum = lift . categoricalEnum

instance
    (Monad m, DeterministicPrimitives d i b mat, Show tt) =>
    CategoricalEnum (PopulationT d m) d tt
    where
    categoricalEnum (ps, vals) = do
        populationT (return (zip vals ps))

instance (Monad m, DeterministicPrimitives d i b mat, Show tt) => CategoricalEnum (ResamplerT resampler d m) d tt where
    categoricalEnum = ResamplerT . (populationT . lift . runPopulationT) . categoricalEnum

instance (Monad m, DeterministicPrimitives d i b mat, Show tt) => CategoricalEnum (LoggedResamplerT resampler logA d m) d tt where
    categoricalEnum = LoggedResamplerT . (populationT . lift . runPopulationT) . categoricalEnum

instance (Monad m, DeterministicPrimitives d i b mat, Show tt) => CategoricalEnum (WeightedResamplerT resampler d m) d tt where
    categoricalEnum = WeightedResamplerT . (weightedPopulationT . lift . runWeightedPopulationT) . categoricalEnum

instance
    (Monad m, DeterministicPrimitives d i b mat, Show tt) =>
    CategoricalEnum (WeightedPopulationT d m) d tt
    where
    categoricalEnum = WeightedPopulationT . categoricalEnum

instance (CategoricalEnum m1 d i, CategoricalEnum m2 d i) => CategoricalEnum (Product m1 m2) d i where
    categoricalEnum (ps, vals) = Pair (categoricalEnum (ps, vals)) (categoricalEnum (ps, vals))
