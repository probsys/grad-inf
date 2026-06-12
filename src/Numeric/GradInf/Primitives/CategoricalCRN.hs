{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.CategoricalCRN where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)
import Prelude hiding (flip)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.Categorical
import Numeric.GradInf.Primitives.CategoricalEnum
import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Class definition

class (Monad m) => CategoricalCRN m r tt where
    categoricalCRN :: ([r], [tt]) -> m tt

-- Core semantics

instance CategoricalCRN Sampler Double tt where
    categoricalCRN = categorical

-- Supporting inference

instance
    (Categorical Sampler (Expected d d) tt) =>
    CategoricalCRN Sampler (Expected d d) tt
    where
    categoricalCRN = categorical

-- Support coupling + partial eval

instance
    ( Monad m0
    , DeterministicPrimitives d i b mat
    , GetIndex tt i
    , Categorical Sampler d i
    , CategoricalEnum m0 d tt
    , Mergeable t tt
    , Show tt
    ) =>
    CategoricalCRN (WrapperSamplerT m0) (Coupled d) tt
    where
    categoricalCRN (ps, vals) = do
        let psA = map (\(Coupled (pA, pB)) -> pA) ps
        let indices :: [i] = map fromInt [0 .. (length psA - 1)]
        iA <- primal (categorical (psA, indices))
        let xA :: tt = getIndex vals iA

        let cdfsA = scanl1 (+) psA
        let cdfLowA = if iA == 0 then 0.0 else (getIndex cdfsA (iA - 1))
        let cdfHighA = getIndex cdfsA iA

        xB <-
            residual
                ( do
                    let psB = map (\(Coupled (pA, pB)) -> pB) ps
                    let cdfsB = scanl1 (+) psB
                    let cdfIntervalsB = zip (0.0 : init cdfsB) cdfsB
                    let safeMin x y = if (isGreater x y) :: b then y else x
                    let safeMax x y = if (isGreater x y) :: b then x else y
                    let computeConditionalPB (cdfLowB, cdfHighB) =
                            max
                                0
                                (safeMin cdfHighB cdfHighA - safeMax cdfLowB cdfLowA)
                                / (cdfHighA - cdfLowA)
                    let conditionalPsB = map computeConditionalPB cdfIntervalsB
                    -- below lines are equivalent to
                    -- categoricalEnum (conditionalPsB)
                    -- but with zero-weight particles dropped.

                    let nonZeroConditionalPsAndVals = filter ((\x -> extractBool (isGreater x 0 :: b)) . fst) (zip conditionalPsB vals)
                    let nonZeroConditionalPs = map fst nonZeroConditionalPsAndVals
                    let nonZeroConditionalVals = map snd nonZeroConditionalPsAndVals

                    xB <- categoricalEnum (nonZeroConditionalPs, nonZeroConditionalVals)
                    return xB
                )

        return (merge xA xB)
