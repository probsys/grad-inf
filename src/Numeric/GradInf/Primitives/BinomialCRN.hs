module Numeric.GradInf.Primitives.BinomialCRN where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.Binomial
import Numeric.GradInf.Primitives.CategoricalCRN
import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Class definition

class (Monad m) => BinomialCRN m r i where
    binomialCRN :: (i, r) -> m i

-- Core semantics

instance BinomialCRN Sampler Double Int where
    binomialCRN = binomial

-- Supporting inference

instance
    (Binomial Sampler (Expected d d) (Expected d i)) =>
    BinomialCRN Sampler (Expected d d) (Expected d i)
    where
    binomialCRN = binomial

-- Support coupling + partial eval

instance
    (Monad m0, CategoricalCRN (WrapperSamplerT m0) (Coupled Double) (Coupled Int)) =>
    BinomialCRN (WrapperSamplerT m0) (Coupled Double) (Coupled Int)
    where
    binomialCRN ((Coupled (nA, nB), Coupled (pA, pB))) =
        let
            psA = map (binomialGetP (nA, pA)) [0 .. nA] ++ (replicate (max (nB - nA) 0) 0.0)
            psB = map (binomialGetP (nB, pB)) [0 .. nB] ++ (replicate (max (nA - nB) 0) 0.0)
            ps = map (\(pA, pB) -> Coupled (pA, pB)) (zip psA psB)
            indices :: [Coupled Int] = map fromInt [0 .. (length ps - 1)]
         in
            categoricalCRN (ps, indices)

instance
    ( Monad m
    , CategoricalCRN
        (WrapperSamplerT m)
        (Coupled (Expected d d))
        (Coupled (Expected d i))
    ) =>
    BinomialCRN
        (WrapperSamplerT m)
        (Coupled (Expected d d))
        (Coupled (Expected d i))
    where
    binomialCRN ((Coupled (Expected nA, Expected nB), Coupled (Expected pA, Expected pB))) = do
        undefined
