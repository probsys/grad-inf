{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.MvNormalMaxReflection where

import Control.Monad.Bayes.Class qualified as Bayes (mvNormal)
import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)
import Data.Matrix qualified as Matrix (diagonal)
import Data.Maybe (fromJust)
import Data.Vector qualified as BoxVector (fromList)
import Data.Vector.Generic qualified as GenericVector (
    length,
    replicate,
    toList,
 )
import Foreign.Storable (Storable)
import Numeric.LinearAlgebra qualified as LA
import Numeric.LinearAlgebra.Data qualified as LA.Data
import Statistics.Distribution (ContDistr (logDensity))
import Statistics.Distribution.Normal (normalDistr)
import Prelude hiding (flip)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.Flip
import Numeric.GradInf.Primitives.FlipEnum

-- Class definition

class (Monad m) => MvNormalMaxReflection m r | m r -> r where
    mvNormalMaxReflection :: ([r], r) -> m [r]

-- Core semantics

instance MvNormalMaxReflection Sampler Double where
    mvNormalMaxReflection (mu, sigma) =
        fmap GenericVector.toList $
            Bayes.mvNormal
                (BoxVector.fromList mu)
                (Matrix.diagonal 0 (GenericVector.replicate (length mu) sigma))

-- Supporting inference

instance (MvNormalMaxReflection Sampler d) => MvNormalMaxReflection Sampler (Expected d d) where
    mvNormalMaxReflection (muExpected, Expected sigma) = fmap (map Expected) (mvNormalMaxReflection (map (\(Expected x) -> x) muExpected, sigma))

-- Support coupling + partial eval

class StdNormalLogPdf d where
    stdNormalLogPdf :: [d] -> d

instance StdNormalLogPdf Double where
    stdNormalLogPdf x = do
        logDensity (normalDistr 0 1) (LA.norm_2 (LA.fromList x))
            - (fromIntegral (length x - 1)) / 2 * log (2 * pi)

instance (StdNormalLogPdf d) => StdNormalLogPdf (Expected d d) where
    stdNormalLogPdf x = Expected (stdNormalLogPdf (map (\(Expected x) -> x) x))

instance
    ( Monad m0
    , DeterministicPrimitives d i b mat
    , MvNormalMaxReflection Sampler d
    , FlipEnum m0 d b
    , Show d
    , Storable d
    , StdNormalLogPdf d
    ) =>
    MvNormalMaxReflection (WrapperSamplerT m0) (Coupled d)
    where
    mvNormalMaxReflection (mu, Coupled (sigmaA, sigmaB)) = do
        if (sigmaA /= sigmaB)
            then
                error
                    ( "MvNormalMaxReflection: only supports sigmaA == sigmaB, got "
                        ++ show (sigmaA, sigmaB)
                    )
            else do
                let muA = map (\(Coupled (muA, muB)) -> muA) mu
                let muB = map (\(Coupled (muA, muB)) -> muB) mu

                xA <- primal (mvNormalMaxReflection (muA, sigmaA))

                let plus = zipWith (+)
                let minus = zipWith (-)
                let scale c x = map (\x -> c * x) x
                let dot x y = sum (zipWith (*) x y)
                let norm2 x = sum (map (\x -> x * x) x)

                let pdfA = exp $ stdNormalLogPdf (xA `minus` muA)
                let pdfB = exp $ stdNormalLogPdf (xA `minus` muB)

                reflect <- residual (flipEnum (max 0 (1 - pdfB / pdfA)))

                let xB =
                        if (reflect :: b)
                            then
                                xA
                                    `plus` ( (1 - 2 * ((muB `minus` muA) `dot` (xA `minus` muA)) / (norm2 (muB `minus` muA)))
                                                `scale` (muB `minus` muA)
                                           )
                            else
                                xA
                
                return (map Coupled (zip xA xB))
