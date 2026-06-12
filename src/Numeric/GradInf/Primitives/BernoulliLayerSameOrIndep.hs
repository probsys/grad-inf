{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.BernoulliLayerSameOrIndep where

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


-- Class definition

class (Monad m) => BernoulliLayerSameOrIndep m mat where
    bernoulliLayerSameOrIndep :: mat -> m mat

-- Core semantics

instance BernoulliLayerSameOrIndep Sampler (Matrix Double) where
    bernoulliLayerSameOrIndep psMat = do
        let ps :: [Double] = matrixToList psMat
        bs :: [Bool] <- mapM flip ps
        let bsMat :: Matrix Double = listToMatrix (map (\b -> if b then 1 else 0) bs :: [Double])
        return bsMat

-- Supporting inference

instance (BernoulliLayerSameOrIndep Sampler mat) => BernoulliLayerSameOrIndep Sampler (Base mat mat) where
    bernoulliLayerSameOrIndep = fmap Base . bernoulliLayerSameOrIndep . (\(Base x) -> x)

instance BernoulliLayerSameOrIndep Sampler (Expected mat mat) where
    bernoulliLayerSameOrIndep = undefined

instance BernoulliLayerSameOrIndep Sampler (Forward (Matrix Double)) where
    bernoulliLayerSameOrIndep = fmap pureForward . (bernoulliLayerSameOrIndep) . extractForwardNum

instance
    (BernoulliLayerSameOrIndep Sampler mat) =>
    BernoulliLayerSameOrIndep Sampler (AD s mat)
    where
    bernoulliLayerSameOrIndep = fmap AD . (bernoulliLayerSameOrIndep) . runAD

instance (Reifies s W) => BernoulliLayerSameOrIndep Sampler (BVar s (Matrix Double)) where
    bernoulliLayerSameOrIndep =
        fmap pureBVar . bernoulliLayerSameOrIndep . extractBVar

-- Support coupling + partial eval

instance
    ( DeterministicPrimitives d i b mat
    , BernoulliLayerSameOrIndep Sampler mat
    , CategoricalEnum m0 d [Coupled b]
    ) =>
    BernoulliLayerSameOrIndep (WrapperSamplerT m0) (Coupled mat)
    where
    bernoulliLayerSameOrIndep (Coupled (psMatA, psMatB)) = do

        let pAs :: [d] = matrixToList psMatA
        let pBs :: [d] = matrixToList psMatB

        bsMatA <- primal (bernoulliLayerSameOrIndep psMatA)
        bsMatB <- primal (bernoulliLayerSameOrIndep psMatA)

        let bAs :: [b] = map (\x -> if (isGreater x 0.5) :: b then fromBool True else fromBool False) (matrixToList bsMatA :: [d])
        let bBs :: [b] = map (\x -> if (isGreater x 0.5) :: b then fromBool True else fromBool False) (matrixToList bsMatB :: [d])

        let ws =
                map
                    ( \(pA, pB, bA, bB) -> do
                        let deltaPA = if bA then pB - pA else pA - pB
                        let deltaPB = if bB then pB - pA else pA - pB
                        let pOutcomeA = if bA then pA else 1 - pA
                        let pOutcomeB = if bB then pA else 1 - pA
                        let w1 = deltaPA / (2 * pOutcomeA)
                        let w2 = deltaPB / (2 * pOutcomeB) - deltaPA / (2 * pOutcomeA)
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
