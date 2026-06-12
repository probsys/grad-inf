module Numeric.GradInf.Primitives.FoldP (FoldrP (..), FoldOnce (..)) where

import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.Primitives.ResampleManual
import Numeric.GradInf.PartialProbabilityEval (PartialMonad (..), WrapperSamplerT (..), getWrapperSamplerT)
import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT)

class FoldrP m a b where
    foldrP :: (a -> b -> b) -> b -> [m a] -> m b

instance (Monad m, FoldOnce m a b b, ResampleManual (KeepLargeWeightsResampler KillSmallWeightsResampler) m b) => FoldrP m a b where
    foldrP f init xs = do
        let resampler = KeepLargeWeightsResampler (KillSmallWeightsResampler 1e-4) 1e-4
        foldr (\ma macc -> resampleManual resampler (foldOnce f ma macc)) (return init) xs

class FoldOnce m a b c where
    foldOnce :: (a -> b -> c) -> m a -> m b -> m c

instance FoldOnce Sampler a b c where
    foldOnce f ma mb = do
        a <- ma
        b <- mb
        return (f a b)

instance (Monad m, DeterministicPrimitives d i b_prim mat) => FoldOnce (PopulationT d m) a b c where
    foldOnce f popA popB = populationT $ do
        p1 :: [(a, d)] <- runPopulationT popA
        p2 :: [(b, d)] <- runPopulationT popB
        runPopulationT $ liftA2 f (populationT (return p1)) (populationT (return p2))

instance (FoldOnce (PopulationT d (StateT (resampler, Int) m)) a b c)
            => FoldOnce (ResamplerT resampler d m) a b c
  where
  foldOnce f r1 r2 =
    (ResamplerT (foldOnce f (getResamplerT r1) (getResamplerT r2)))

instance (Monad m0, FoldOnce m0 a b c) => FoldOnce (WrapperSamplerT m0) a b c where
    foldOnce f wa wb =
        WrapperSamplerT $ do
            ma <- getWrapperSamplerT wa
            mb <- getWrapperSamplerT wb
            getWrapperSamplerT (residual (foldOnce f ma mb))