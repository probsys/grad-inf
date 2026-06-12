{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.GradInf.PartialProbabilityEval where

import Control.Monad (ap)
import Control.Monad.Bayes.Sampler.Lazy (
    Sampler,
    SamplerT (..),
    Tree,
    randomTree,
    runSampler,
 )
import Control.Monad.Identity (Identity (..))
import Prelude hiding (flip)

import System.Random (mkStdGen)

-- Define monad transformer --

seeded :: Tree -> Sampler a -> a
seeded t (SamplerT s) = runIdentity (s t)

getSeed :: Sampler Tree
getSeed = SamplerT $ \g -> (return g)

samplerWithSeed :: Int -> Sampler a -> IO a
samplerWithSeed seed m = pure (runSampler m (randomTree (mkStdGen seed)))

newtype WrapperSamplerT m0 a = WrapperSamplerT (Sampler (m0 a))

getWrapperSamplerT :: WrapperSamplerT m0 a -> Sampler (m0 a)
getWrapperSamplerT (WrapperSamplerT xP) = xP

instance (Monad m0) => Functor (WrapperSamplerT m0) where
    fmap f (WrapperSamplerT xPP) = WrapperSamplerT (fmap (fmap f) xPP)

instance (Monad m0) => Monad (WrapperSamplerT m0) where
    return a = WrapperSamplerT (return (return a))
    WrapperSamplerT xPP >>= f = WrapperSamplerT $ do
        xP <- xPP
        let fbar = do
                s <- getSeed
                return
                    ( \x ->
                        let
                            (WrapperSamplerT fx) = f x
                         in
                            seeded s fx
                    )
        g <- fbar
        return (xP >>= g)

-- Define applicative via the monad
instance (Monad m0) => Applicative (WrapperSamplerT m0) where
    pure = return
    (<*>) = ap

class (Monad m0, Monad m) => PartialMonad m m0 | m -> m0 where
    primal :: Sampler a -> m a
    residual :: m0 a -> m a

instance (Monad m0) => PartialMonad (WrapperSamplerT m0) m0 where
    primal xP = WrapperSamplerT (fmap return xP)
    residual xP = WrapperSamplerT (return xP)

