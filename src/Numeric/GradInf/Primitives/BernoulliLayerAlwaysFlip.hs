{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.BernoulliLayerAlwaysFlip where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)
import Prelude hiding (flip)
import Numeric.LinearAlgebra

import Numeric.GradInf.Coupling
import Numeric.GradInf.AD.AD
import Numeric.GradInf.AD.Backprop
import Numeric.GradInf.Inference.Base
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.CategoricalEnum
import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.Flip
import Numeric.GradInf.Primitives.Resample (Resample (..))

-- Class definition

class (Monad m) => BernoulliLayerAlwaysFlip m mat where
    bernoulliLayerAlwaysFlip :: mat -> m mat

-- Core semantics

instance BernoulliLayerAlwaysFlip Sampler (Matrix Double) where
    bernoulliLayerAlwaysFlip psMat = do
       let ps :: [Double] = matrixToList psMat
       bs :: [Bool] <- mapM flip ps
       let bsMat :: Matrix Double = listToMatrix (map (\b -> if b then 1 else 0) bs :: [Double])
       return bsMat

-- Supporting inference

instance
    (DeterministicPrimitives d i b mat, BernoulliLayerAlwaysFlip Sampler mat) =>
    BernoulliLayerAlwaysFlip Sampler (Base mat mat)
    where
    bernoulliLayerAlwaysFlip = fmap Base . bernoulliLayerAlwaysFlip . (\(Base x) -> x)

instance
    (DeterministicPrimitives d i b mat, BernoulliLayerAlwaysFlip Sampler mat) =>
    BernoulliLayerAlwaysFlip Sampler (Expected mat mat)
    where
    bernoulliLayerAlwaysFlip = do
        fmap Expected . bernoulliLayerAlwaysFlip . (\(Expected x) -> x)

instance
    (BernoulliLayerAlwaysFlip Sampler (Matrix Double)) =>
    BernoulliLayerAlwaysFlip Sampler (Forward (Matrix Double))
    where
    bernoulliLayerAlwaysFlip =
        fmap pureForward
            . bernoulliLayerAlwaysFlip
            . extractForwardNum

instance
    (BernoulliLayerAlwaysFlip Sampler mat) =>
    BernoulliLayerAlwaysFlip Sampler (AD s mat)
    where
    bernoulliLayerAlwaysFlip = fmap AD . bernoulliLayerAlwaysFlip . runAD

instance
    (Reifies s W, BernoulliLayerAlwaysFlip Sampler (Matrix Double)) =>
    BernoulliLayerAlwaysFlip Sampler (BVar s (Matrix Double))
    where
    bernoulliLayerAlwaysFlip =
        fmap pureBVar
            . bernoulliLayerAlwaysFlip
            . extractBVar

-- Support coupling + partial eval

instance
    ( DeterministicPrimitives d i b mat
    , Monad m0
    , BernoulliLayerAlwaysFlip Sampler mat
    , CategoricalEnum m0 d [Coupled b]
    , Resample m0 [Coupled b]
    ) =>
    BernoulliLayerAlwaysFlip (WrapperSamplerT m0) (Coupled mat)
    where
    bernoulliLayerAlwaysFlip (Coupled (psMatA, psMatB)) = do

        let pAs :: [d] = matrixToList psMatA
        let pBs :: [d] = matrixToList psMatB

        bsMatA <- primal (bernoulliLayerAlwaysFlip psMatA)
        let bAs :: [b] = map (\x -> if (isGreater x 0.5) :: b then fromBool True else fromBool False) (matrixToList bsMatA :: [d])

        bs :: [Coupled b] <-
            foldl
                ( \z (xA, pA, pB) ->
                    resample $ z
                        >>= ( \xBs ->
                                do
                                    let w = if (xA :: b) then (pA - pB) else (pB - pA)
                                    let xB :: b = if (xA :: b) then (fromBool False) else (fromBool True)
                                    residual
                                        ( categoricalEnum
                                            ([1 - w, w], [(xBs ++ [Coupled (xA, xA)]), (xBs ++ [Coupled (xA, xB)])])
                                        )
                            )
                )
                (return [] :: (WrapperSamplerT m0) [Coupled b])
                (zip3 bAs pAs pBs)

        let bsMat1 = listToMatrix (map (\(Coupled (bA, _)) -> if bA then 1 else 0) bs :: [d])
        let bsMat2 = listToMatrix (map (\(Coupled (_, bB)) -> if bB then 1 else 0) bs :: [d])

        return (Coupled (bsMat1, bsMat2))
