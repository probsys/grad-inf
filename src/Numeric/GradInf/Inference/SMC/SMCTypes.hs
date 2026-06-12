{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.GradInf.Inference.SMC.SMCTypes where

import Data.Functor.Product
import Control.Applicative (Alternative)
import Control.Monad (forM)
import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.Bayes.Class qualified as Bayes (
    bernoulli,
    categorical,
    random,
 )
import Control.Monad.State (StateT (..), modify)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Functor.Compose
import Data.List (partition)
import Prelude hiding (flip)

import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Building SMC monad transformer --

class (Monad m) => MonadFactor d m where
    score :: d -> m ()

class (MonadDistribution m, MonadFactor d m) => MonadMeasure d m

-- ListT

newtype ListT m a = ListT {getListT :: Compose m [] a}
    deriving newtype (Functor, Applicative, Alternative)

listT :: m [a] -> ListT m a
listT = ListT . Compose

runListT :: ListT m a -> m [a]
runListT = getCompose . getListT

instance (Monad m) => Monad (ListT m) where
    ma >>= f = ListT $ Compose $ do
        as <- runListT ma
        fmap concat $ forM as $ runListT . f

instance MonadTrans ListT where
    lift = ListT . Compose . fmap pure

instance (MonadDistribution m) => MonadDistribution (ListT m) where
    random = lift Bayes.random
    bernoulli = lift . Bayes.bernoulli
    categorical = lift . Bayes.categorical

instance (MonadFactor d m) => MonadFactor d (ListT m) where
    score = lift . score

instance (MonadMeasure d m) => MonadMeasure d (ListT m)

-- WeightedT

newtype WeightedT d m a = WeightedT (StateT d m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadDistribution)

runWeightedT :: (DeterministicPrimitives d i b mat) => WeightedT d m a -> m (a, d)
runWeightedT (WeightedT m) = runStateT m 1

weightedT ::
    (Monad m, DeterministicPrimitives d i b mat) => m (a, d) -> WeightedT d m a
weightedT m = WeightedT $ do
    (x, w) <- lift m
    modify (safeMul w)
    return x

instance (Monad m, DeterministicPrimitives d i b mat) => MonadFactor d (WeightedT d m) where
    score w = WeightedT (modify (safeMul w))

instance
    (MonadDistribution m, DeterministicPrimitives d i b mat) =>
    MonadMeasure d (WeightedT d m)

-- PopulationT

newtype PopulationT d m a = PopulationT (WeightedT d (ListT m) a)
    deriving newtype
        (Functor, Applicative, Monad, MonadDistribution)

instance
    (Monad m, MonadFactor d (WeightedT d (ListT m))) =>
    MonadFactor d (PopulationT d m)
    where
    score x = PopulationT (score x)

instance
    (MonadDistribution m, MonadMeasure d (WeightedT d (ListT m))) =>
    MonadMeasure d (PopulationT d m)

runPopulationT ::
    (DeterministicPrimitives d i b mat) => PopulationT d m a -> m [(a, d)]
runPopulationT (PopulationT m) = runListT (runWeightedT m)

populationT ::
    (Monad m, DeterministicPrimitives d i b mat) => m [(a, d)] -> PopulationT d m a
populationT = PopulationT . weightedT . listT

instance MonadTrans (PopulationT d) where
    lift = PopulationT . lift . lift

-- ResamplerT

class DoResample resampler d m a where
    doResample :: resampler -> Int -> [(a, d)] -> m [(a, d)]

newtype ResamplerT resampler d m a = ResamplerT
    { getResamplerT :: (PopulationT d (StateT (resampler, Int) m)) a
    }
    deriving newtype
        (Functor, Applicative, Monad, MonadDistribution)

instance (Monad m, MonadFactor d (PopulationT d (StateT (resampler, Int) m))) => MonadFactor d (ResamplerT resampler d m) where
    score x = ResamplerT (score x)

instance (MonadDistribution m, MonadMeasure d (PopulationT d (StateT (resampler, Int) m))) => MonadMeasure d (ResamplerT resampler d m)

-- LoggedResamplerT
-- Like ResamplerT, but also accumulates the population at each resampling step.
-- logA is the type of value to log (may differ from `a` via a projection).

newtype LoggedResamplerT resampler logA d m a =
    LoggedResamplerT ((PopulationT d (StateT (resampler, Int, [[(logA, d)]]) m)) a)
    deriving newtype
        (Functor, Applicative, Monad, MonadDistribution)

instance (Monad m, MonadFactor d (PopulationT d (StateT (resampler, Int, [[(logA, d)]]) m))) => MonadFactor d (LoggedResamplerT resampler logA d m) where
    score x = LoggedResamplerT (score x)

instance (MonadDistribution m, MonadMeasure d (PopulationT d (StateT (resampler, Int, [[(logA, d)]]) m))) => MonadMeasure d (LoggedResamplerT resampler logA d m)

newtype WeightedPopulationT d m a = WeightedPopulationT {getWeightedPopulationT :: WeightedT d (PopulationT d m) a}
    deriving newtype
        (Functor, Applicative, Monad, MonadDistribution)

runWeightedPopulationT :: (DeterministicPrimitives d i b mat) => WeightedPopulationT d m a -> m [((a, d), d)]
runWeightedPopulationT (WeightedPopulationT m) = runPopulationT (runWeightedT m)

weightedPopulationT :: (Monad m, DeterministicPrimitives d i b mat) => m [((a, d), d)] -> WeightedPopulationT d m a
weightedPopulationT = WeightedPopulationT . weightedT . populationT


instance (Monad m, MonadFactor d (WeightedT d (PopulationT d m))) => MonadFactor d (WeightedPopulationT d m) where
    score x = WeightedPopulationT (score x)

instance (MonadDistribution m, MonadMeasure d (WeightedT d (PopulationT d m))) => MonadMeasure d (WeightedPopulationT d m)

-- WeightedResamplerT
--
-- Two-weight populations @ [((a, d), d)] @ are handled as ordinary @DoResample@ with particle type @(a, d)@.

newtype WeightedResamplerT resampler d m a = WeightedResamplerT ((WeightedPopulationT d (StateT (resampler, Int) m)) a)
    deriving newtype
        (Functor, Applicative, Monad, MonadDistribution)

instance (Monad m, MonadFactor d (WeightedPopulationT d (StateT (resampler, Int) m))) => MonadFactor d (WeightedResamplerT resampler d m) where
    score x = WeightedResamplerT (score x)

instance (MonadDistribution m, MonadMeasure d (WeightedPopulationT d (StateT (resampler, Int) m))) => MonadMeasure d (WeightedResamplerT resampler d m)

-- StateT

instance (MonadFactor d m) => MonadFactor d (StateT s m) where
    score = lift . score

instance (MonadMeasure d m) => MonadMeasure d (StateT s m)

-- Product

instance (MonadFactor d m1, MonadFactor d m2) => MonadFactor d (Product m1 m2) where
    score w = Pair (score w) (score w)

instance (MonadDistribution m1, MonadDistribution m2) => MonadDistribution (Product m1 m2) where
    random = Pair Bayes.random Bayes.random
    bernoulli p = Pair (Bayes.bernoulli p) (Bayes.bernoulli p)
    categorical ps = Pair (Bayes.categorical ps) (Bayes.categorical ps)

instance (MonadMeasure d m1, MonadMeasure d m2) => MonadMeasure d (Product m1 m2)