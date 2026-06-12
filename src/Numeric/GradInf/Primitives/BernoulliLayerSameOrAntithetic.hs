{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.BernoulliLayerSameOrAntithetic where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)
import Data.List (zip4)
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
import Numeric.GradInf.Primitives.Uniform


-- Class definition

class (Monad m) => BernoulliLayerSameOrAntithetic m mat where
    bernoulliLayerSameOrAntithetic :: mat -> m mat

-- Core semantics

instance BernoulliLayerSameOrAntithetic Sampler (Matrix Double) where
    bernoulliLayerSameOrAntithetic psMat = do
       let ps :: [Double] = matrixToList psMat
       bs :: [Bool] <- mapM flip ps
       let bsMat :: Matrix Double = listToMatrix (map (\b -> if b then 1 else 0) bs :: [Double])
       return bsMat

-- Supporting inference

instance (BernoulliLayerSameOrAntithetic Sampler mat) => BernoulliLayerSameOrAntithetic Sampler (Base mat mat) where
    bernoulliLayerSameOrAntithetic = fmap Base . bernoulliLayerSameOrAntithetic . (\(Base x) -> x)

instance BernoulliLayerSameOrAntithetic Sampler (Expected mat mat) where
    bernoulliLayerSameOrAntithetic = undefined

instance BernoulliLayerSameOrAntithetic Sampler (Forward (Matrix Double)) where
    bernoulliLayerSameOrAntithetic = fmap pureForward . (bernoulliLayerSameOrAntithetic) . extractForwardNum

instance
    (BernoulliLayerSameOrAntithetic Sampler mat) =>
    BernoulliLayerSameOrAntithetic Sampler (AD s mat)
    where
    bernoulliLayerSameOrAntithetic = fmap AD . (bernoulliLayerSameOrAntithetic) . runAD

instance (Reifies s W) => BernoulliLayerSameOrAntithetic Sampler (BVar s (Matrix Double)) where
    bernoulliLayerSameOrAntithetic =
        fmap pureBVar . bernoulliLayerSameOrAntithetic . extractBVar

-- Support coupling + partial eval

instance
    ( DeterministicPrimitives d i b mat
    , BernoulliLayerSameOrAntithetic Sampler mat
    , Uniform Sampler d
    , CategoricalEnum m0 d [Coupled b] -- , Show d, Show b) =>
    , Show b
    ) =>
    BernoulliLayerSameOrAntithetic (WrapperSamplerT m0) (Coupled mat)
    where
    bernoulliLayerSameOrAntithetic (Coupled (psMatA, psMatB)) = do

        let pAs :: [d] = matrixToList psMatA
        let pBs :: [d] = matrixToList psMatB

        us :: [d] <- primal (mapM (const uniform) [1 .. (length pAs)])
        let bAs = map (\(pA, u) -> fromBool (u < pA)) (zip pAs us)
        let bBs = map (\(pA, u) -> fromBool (u > 1 - pA)) (zip pAs us)

        let ws =
                map
                    ( \(pA, pB, bA, bB) -> do
                        let isDiff = if bA then (if bB then 0 else 1) else (if bB then 1 else 0)
                        let deltaP = if bA then pB - pA else pA - pB
                        let minP = if (isGreater pA (1 / 2)) :: b then 1 - pA else pA
                        let pOutcomeA = if bA then pA else 1 - pA
                        let w1 = deltaP / (2 * pOutcomeA)
                        let w2 = -deltaP / (2 * minP) * isDiff
                        (w1, w2)
                    )
                    (zip4 pAs pBs bAs bBs)

        let w1 = sum (map fst ws)
        let w2 = sum (map snd ws)

        let b1 = map (\bA -> Coupled (bA, bA)) bAs
        let b2 = map (\(bA, bB) -> Coupled (bA, bB)) (zip bAs bBs)

        bs :: [Coupled b] <- residual (categoricalEnum ([1 + w1, w2], [b1, b2]))

        let bsMat1 = listToMatrix (map (\(Coupled (bA, _)) -> if bA then 1 else 0) bs :: [d])
        let bsMat2 = listToMatrix (map (\(Coupled (_, bB)) -> if bB then 1 else 0) bs :: [d])

        return (Coupled (bsMat1, bsMat2))
