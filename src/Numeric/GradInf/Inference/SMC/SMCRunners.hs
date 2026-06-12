{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.GradInf.Inference.SMC.SMCRunners where

import Data.List (nub, partition)
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class (MonadDistribution)
import Control.Monad.State (StateT (..))
import Numeric.GradInf.Inference.SMC.SMCTypes
import Numeric.GradInf.Primitives.Categorical
import Numeric.GradInf.Primitives.DeterministicPrimitives
import Prelude hiding (flip)
import Numeric.GradInf.Coupling

-- Resampler logic --

-- MultinomialResampler

data MultinomialResampler = MultinomialResampler Int

instance (MonadDistribution m, DeterministicPrimitives d i b mat, Categorical m d (a, d)) =>
    DoResample MultinomialResampler d m a where
    doResample (MultinomialResampler n)_ pop = do
        let (xs, ws) = unzip pop
        -- we don't use abs here because it drops dual components when w = 0
        let safeAbs w = if (isGreater w 0) :: b then w else -w
        let z = sum (map safeAbs ws)
        let ps = map (\w -> safeDiv (safeAbs w) z) ws
        if (extractBool (isGreater (sum ps) 0 :: b))
            then
                replicateM
                    n
                    ( do
                        (x, w) <- categorical (ps, zip xs ws)
                        let signW = if (isGreater w 0) :: b then 1 else -1
                        let out = (x, z / fromIntegral n * signW)
                        return out
                    )
            else
                return []

-- TwoSidedResampler

data TwoSidedResampler positiveResampler negativeResampler = TwoSidedResampler {positiveResampler :: positiveResampler, negativeResampler :: negativeResampler}

instance (MonadDistribution m, DeterministicPrimitives d i b mat, DoResample positiveResampler d m a, DoResample negativeResampler d m a, Show a) =>
    DoResample (TwoSidedResampler positiveResampler negativeResampler) d m a where
    doResample (TwoSidedResampler positiveResampler negativeResampler) i pop = do
        let positiveToResample = filter ((\w -> extractBool (isGreater w 0 :: b)) . snd) pop
        let negativeToResample = filter ((\w -> extractBool (isGreater 0 w :: b)) . snd) pop
        positiveOffsprings <- doResample positiveResampler i positiveToResample
        negativeOffsprings <- doResample negativeResampler i negativeToResample
        return (positiveOffsprings ++ negativeOffsprings)

-- KeepLargeWeightsResampler

data KeepLargeWeightsResampler resampler = KeepLargeWeightsResampler {resampler :: resampler, threshold :: Double}

instance (Monad m, DeterministicPrimitives d i b mat, DoResample resampler d m a, Show a) => DoResample (KeepLargeWeightsResampler resampler) d m a where
    doResample (KeepLargeWeightsResampler resampler threshold) i pop = do
        let (_, ws) = unzip pop
        let z = sum (map abs ws)
        let (largeOffsprings, smallToResample) = partition ((> (fromDouble threshold * z)) . abs . snd) pop
        smallOffsprings <- doResample resampler i smallToResample
        let finalPop = (largeOffsprings ++ smallOffsprings)
        return finalPop

-- KillSmallWeightsResampler

data KillSmallWeightsResampler = KillSmallWeightsResampler {threshold :: Double}

instance (Monad m, DeterministicPrimitives d i b mat) => DoResample KillSmallWeightsResampler d m a where
    doResample (KillSmallWeightsResampler threshold) _ pop = do
        let safeAbs w = if (isGreater w 0) :: b then w else -w
        let z = sum (map safeAbs (map snd pop))
        let keep (_, w) = not (extractBool (isGreater (fromDouble threshold * z) (safeAbs w) :: b))
        return (filter keep pop)

-- KeepDistinctResampler

data KeepDistinctResampler = KeepDistinctResampler

instance (DeterministicPrimitives d i b mat, Monad m) => DoResample KeepDistinctResampler d m (Coupled i) where
    doResample KeepDistinctResampler _ pop = do
        let safeAbs w = if (isGreater w 0) :: b then w else -w
        let isNonZero w = extractBool (isGreater (safeAbs w) 0 :: b)
        let (xs, ws) = unzip pop
        let xsPaired = map (\(Coupled (xL, xR)) -> (xL, xR)) xs
        let xsPaired' = nub xsPaired
        let xs' = map Coupled xsPaired'
        let ws' = map (\x' -> sum (map snd (filter (\(x, w) -> isNonZero w && x == x') (zip xsPaired ws)))) xsPaired'
        let pop' = zip xs' ws'
        let pop'' = filter (\(_, w) -> isNonZero w) pop'
        return pop''

-- TwistedResampler

data TwistedResampler resampler d a = TwistedResampler {resampler :: resampler, twist :: Int -> a -> d}

instance (Monad m, DeterministicPrimitives d i b mat, DoResample resampler d m a, Show a) => DoResample (TwistedResampler resampler d a) d m a where
    doResample (TwistedResampler resampler twist) i pop = do
        let (xs, ws) = unzip pop
        let ts = map (twist i) xs
        let wsTwisted = zipWith safeMul ws ts
        let popTwisted = zip xs wsTwisted
        popTwisted' <- doResample resampler i popTwisted
        do
            let (xs', wsTwisted') = unzip popTwisted'
            let ts' = map (twist i) xs'
            let ws' = zipWith safeDiv wsTwisted' ts'
            let pop' = zip xs' ws'
            return pop'

-- ComposeWeightedResampler

data ComposeWeightedResampler resampler1 resampler2 = ComposeWeightedResampler resampler1 resampler2

instance
    (Monad m, DoResample resampler1 d m (a, d), DoResample resampler2 d m (a, d)) =>
    DoResample (ComposeWeightedResampler resampler1 resampler2) d m (a, d)
    where
    doResample (ComposeWeightedResampler resampler1 resampler2) i pop = do
        pop' <- doResample resampler1 i pop
        doResample resampler2 i pop'

-- PropagateSmallWeightsWeightedResampler

-- This weighted resampler finds the small outer weights and propagates this
-- information into their inner weights.
data PropagateSmallWeightsWeightedResampler d =
    PropagateSmallWeightsWeightedResampler {threshold :: d}

instance
    (DeterministicPrimitives d i b mat, Monad m) =>
    DoResample (PropagateSmallWeightsWeightedResampler d) d m (a, d)
    where
    doResample (PropagateSmallWeightsWeightedResampler threshold) _ pop = do
        let (_, ws) = unzip pop
        let safeAbs w = if (isGreater w 0) :: b then w else -w
        let z = sum (map safeAbs ws)
        let (largeOffsprings, smallOffsprings) =
                partition
                    ((\w -> extractBool (isGreater (safeAbs w) (threshold * z) :: b)) . snd)
                    pop
        let smallOffspringsPropagated =
                map
                    (\((x, wIn), wOut) -> ((x, wIn * threshold * z), wOut / (threshold * z)))
                    smallOffsprings
        return (largeOffsprings ++ smallOffspringsPropagated)

-- RunInnerWeightedResampler

data RunInnerWeightedResampler resampler d = RunInnerWeightedResampler {resampler :: resampler}

swapWeights :: [((a, d), d)] -> [((a, d), d)]
swapWeights pop = do
    let (xs', wsOut) = unzip pop
    let (xs, wsIn) = unzip xs'
    zip (zip xs wsOut) wsIn

instance
    (Monad m, DoResample resampler d m (a, d)) =>
    DoResample (RunInnerWeightedResampler resampler d) d m (a, d)
    where
    doResample (RunInnerWeightedResampler resampler) i pop = do
        let popSwapped = swapWeights pop
        popSwapped' <- doResample resampler i popSwapped
        return (swapWeights popSwapped')
