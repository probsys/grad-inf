{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Numeric.GradInf.Primitives.ResampleManual where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT (..))
import Data.Functor.Product

import Numeric.GradInf.Inference.Sequential
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.DeterministicPrimitives

class (Monad m) => ResampleManual resampler m a where
    resampleManual :: resampler -> m a -> m a

instance ResampleManual resampler Sampler a where
    resampleManual _ = id

instance (Monad m) => ResampleManual resampler (PopulationT d m) a where
    resampleManual _ = id

instance
    (DeterministicPrimitives d i b mat, Monad m, DoResample resampler' d m a, Show a) =>
    ResampleManual resampler' (ResamplerT resampler d m) a
    where
    resampleManual rManual (ResamplerT mPop0) = ResamplerT $ populationT $ StateT $ \(resampler, i) -> do
        (pop, (_, j)) <- runStateT (runPopulationT mPop0) (resampler, i)
        pop' <- doResample rManual j pop
        return (pop', (resampler, j + 1))

instance
    ( DeterministicPrimitives d i b mat
    , Monad m
    , DoResample resampler' d m (a, d)
    , Show (a, d)
    ) =>
    ResampleManual resampler' (WeightedResamplerT resampler d m) a
    where
    resampleManual rManual (WeightedResamplerT mPop0) =
        WeightedResamplerT $
            weightedPopulationT $
                StateT $ \(resampler, i) -> do
                    (pop, (_, j)) <- runStateT (runWeightedPopulationT mPop0) (resampler, i)
                    pop' <- doResample rManual j pop
                    return (pop', (resampler, j + 1))

instance
    (DeterministicPrimitives d i b mat, Monad m, DoResample resampler' d m a, Show a) =>
    ResampleManual resampler' (LoggedResamplerT resampler a d m) a
    where
    resampleManual rManual (LoggedResamplerT mPop0) = LoggedResamplerT $ populationT $ StateT $ \(resampler, i, log) -> do
        (pop, (_, j, innerLog)) <- runStateT (runPopulationT mPop0) (resampler, i, log)
        pop' <- doResample rManual j pop
        return (pop', (resampler, j + 1, innerLog ++ [pop]))

instance ResampleManual resampler (Sequential susp) susp where
    resampleManual _ = suspend

instance (ResampleManual resampler m1 a, ResampleManual resampler m2 a) => ResampleManual resampler (Product m1 m2) a where
    resampleManual r (Pair m1 m2) = Pair (resampleManual r m1) (resampleManual r m2)

instance (ResampleManual resampler m a) => ResampleManual resampler (WrapperSamplerT m) a where
    resampleManual r (WrapperSamplerT mu) = WrapperSamplerT $ fmap (resampleManual r) mu
