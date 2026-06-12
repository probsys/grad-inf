module Numeric.GradInf.Primitives.CategoricalIndep where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.Categorical
import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Class definition

class (Monad m) => CategoricalIndep m r tt where
    categoricalIndep :: ([r], [tt]) -> m tt

-- Core semantics

instance CategoricalIndep Sampler Double tt where
    categoricalIndep = categorical

-- Supporting inference

instance
    (Categorical Sampler (Expected d d) tt) =>
    CategoricalIndep Sampler (Expected d d) tt
    where
    categoricalIndep = categorical

-- Support coupling + partial eval

instance
    ( Monad m0
    , DeterministicPrimitives d i b mat
    , GetIndex tt i
    , Categorical Sampler d i
    , Categorical m0 d i
    , Mergeable t tt
    ) =>
    CategoricalIndep (WrapperSamplerT m0) (Coupled d) tt
    where
    categoricalIndep (ps, vals) = do
        let psA = map (\(Coupled (pA, pB)) -> pA) ps
        let indices :: [i] = map fromInt [0 .. (length psA - 1)]
        iA <- primal (categorical (psA, indices))

        iB <-
            residual
                ( do
                    let psB = map (\(Coupled (pA, pB)) -> pB) ps
                    iB <- categorical (psB, indices)
                    return iB
                )

        return (merge (getIndex vals iA) (getIndex vals iB))
