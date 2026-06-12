module Numeric.GradInf.Primitives.BinomialScore where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.Binomial
import Numeric.GradInf.Primitives.CategoricalScore
import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Class definition

class (Monad m) => BinomialScore m r i where
    binomialScore :: (i, r) -> m i

-- Core semantics

instance BinomialScore Sampler Double Int where
    binomialScore = binomial

-- Supporting inference

instance BinomialScore Sampler (Expected d d) (Expected d i) where
    binomialScore = binomial

-- Support coupling + partial eval

instance
    ( DeterministicPrimitives d i b mat
    , MonadFactor d m
    , Show i
    , CategoricalScore (WrapperSamplerT m) (Coupled d) (Coupled i)
    , BinomialGetP d i
    ) =>
    BinomialScore (WrapperSamplerT m) (Coupled d) (Coupled i)
    where
    binomialScore ((Coupled (nA, nB), Coupled (pA, pB))) =
        if nA < nB
            then
                error
                    ( "binomialScore: only correct for nA >= nB (nA = "
                        ++ show nA
                        ++ ", nB = "
                        ++ show nB
                        ++ ")"
                    )
            else
                let
                    psA =
                        (map (binomialGetP (nA, pA)) [0 .. nA])
                            ++ (replicate (max (fromIntegral (nB - nA)) 0) 0.0)
                    psB =
                        (map (binomialGetP (nB, pB)) [0 .. nB])
                            ++ (replicate (max (fromIntegral (nA - nB)) 0) 0.0)
                    ps = map (\(pA, pB) -> Coupled (pA, pB)) (zip psA psB)
                    indices = map fromInt [0 .. ((length ps) - 1)]
                 in
                    categoricalScore (ps, indices)
