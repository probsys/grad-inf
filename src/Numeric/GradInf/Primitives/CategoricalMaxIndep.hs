{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.CategoricalMaxIndep where

import Prelude hiding (flip)
import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.Categorical
import Numeric.GradInf.Primitives.CategoricalEnum
import Numeric.GradInf.Primitives.FlipEnum
import Numeric.GradInf.Primitives.DeterministicPrimitives


-- Class definition

class (Monad m) => CategoricalMaxIndep m r tt where
    categoricalMaxIndep :: ([r], [tt]) -> m tt

-- Core semantics

instance CategoricalMaxIndep Sampler Double tt where
    categoricalMaxIndep = categorical

-- Supporting inference

instance
    (Categorical Sampler (Expected d d) tt) =>
    CategoricalMaxIndep Sampler (Expected d d) tt
    where
    categoricalMaxIndep = categorical

-- Support coupling + partial eval

instance
    ( Monad m0
    , DeterministicPrimitives d i b mat
    , GetIndex tt i
    , Categorical Sampler d i
    , CategoricalEnum m0 d tt
    , FlipEnum m0 d b
    , Mergeable t tt
    , Show d
    , BranchingConstructs b tt
    ) =>
    CategoricalMaxIndep (WrapperSamplerT m0) (Coupled d) tt
    where
    categoricalMaxIndep (ps, vals) = do
        let psA = map (\(Coupled (pA, pB)) -> pA) ps
        let indices :: [i] = map fromInt [0 .. (length psA - 1)]
        iA <- primal (categorical (psA, indices))
        let xA :: tt = getIndex vals iA

        xB <-
            residual
                ( do
                    let psB = map (\(Coupled (pA, pB)) -> pB) ps

                    let safeMin x y = if (isGreater x y) :: b then y else x
                    let safeMax x y = if (isGreater x y) :: b then x else y

                    let agreementProb = safeMin 1.0 (safeDiv (getIndex psB iA) (getIndex psA iA))

                    let safeEquals x y = not (extractBool (isGreater x y :: b)) && not (extractBool (isGreater y x :: b))
                    if safeEquals agreementProb 1 then
                        return xA
                    else do

                        let extraProbs = map (\(pA, pB) -> safeMax 0.0 (pB - pA)) (zip psA psB)
                        let tvDistance = sum extraProbs
                        let conditionalPsB = map (\x -> if (isGreater tvDistance 0) :: b then safeDiv x tvDistance else 0) extraProbs

                        if abs (sum conditionalPsB - 1.0) > 1e-8
                            then
                                error ("categoricalMaxIndep: unexpected error, sum of conditional probs is not 1 (sum = "
                                    ++ show (sum conditionalPsB)
                                    ++ ", agreementProb = "
                                    ++ show (agreementProb)
                                    ++ ")")
                            else do
                                -- below lines are equivalent to
                                -- categoricalEnum (conditionalPsB)
                                -- but with zero-weight particles dropped.

                                let nonZeroConditionalPsAndVals = filter ((\x -> extractBool (isGreater x 0 :: b)) . fst) (zip conditionalPsB vals)
                                let nonZeroConditionalPs = map fst nonZeroConditionalPsAndVals
                                let nonZeroConditionalVals = map snd nonZeroConditionalPsAndVals

                                xB <- categoricalEnum (nonZeroConditionalPs, nonZeroConditionalVals)
                                b :: b <- flipEnum agreementProb
                                return (if b :: b then xA else xB)
            )

        return (merge xA xB)
