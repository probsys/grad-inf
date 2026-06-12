{-# LANGUAGE RankNTypes #-}

module Numeric.GradInf.Primitives.Resample where

import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Bayes.Class qualified as Bayes (random)
import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT(..))
import Data.Functor.Product

import Numeric.GradInf.Inference.Sequential
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.DeterministicPrimitives
-- Class definition

-- A primitive for manually calling a resampling step on the current monad.
class (Monad m) => Resample m a where
    resample :: m a -> m a

-- Core semantics

instance Resample Sampler a where
    resample = id

instance (Monad m) => Resample (PopulationT d m) a where
    resample = id

-- Avoid do notation for reader monad here because of Haskell do-notation bug when
-- reader state is a polymorphic type
instance
    (DeterministicPrimitives d i b mat, Monad m, DoResample resampler d m a, Show a) =>
    Resample (ResamplerT resampler d m) a
    where
    resample (ResamplerT mPop0) = ResamplerT $ populationT $ StateT $ \(resampler, i) -> do
        (pop, (_, j)) <- runStateT (runPopulationT mPop0) (resampler, i)
        pop' <- doResample resampler j pop
        return (pop', (resampler, j + 1))

instance
    ( DeterministicPrimitives d i b mat
    , Monad m
    , DoResample resampler d m (a, d)
    , Show (a, d)
    ) =>
    Resample (WeightedResamplerT resampler d m) a
    where
    resample (WeightedResamplerT mPop0) =
        WeightedResamplerT $
            weightedPopulationT $
                StateT $ \(resampler, i) -> do
                    (pop, (_, j)) <- runStateT (runWeightedPopulationT mPop0) (resampler, i)
                    pop' <- doResample resampler j pop
                    return (pop', (resampler, j + 1))

instance
    (DeterministicPrimitives d i b mat, Monad m, DoResample resampler d m a, Show a) =>
    Resample (LoggedResamplerT resampler a d m) a
    where
    resample (LoggedResamplerT mPop0) = LoggedResamplerT $ populationT $ StateT $ \(resampler, i, log) -> do
        (pop, (_, j, innerLog)) <- runStateT (runPopulationT mPop0) (resampler, i, log)
        pop' <- doResample resampler j pop
        return (pop', (resampler, j + 1, innerLog ++ [pop]))

instance Resample (Sequential susp) susp where
    resample = suspend

instance (Resample m1 a, Resample m2 a) => Resample (Product m1 m2) a where
    resample (Pair m1 m2) = Pair (resample m1) (resample m2)

-- Liftings

-- Apply resampling to residual distribution
instance (Resample m a) => Resample (WrapperSamplerT m) a where
    resample (WrapperSamplerT mu) = WrapperSamplerT $ fmap resample mu
