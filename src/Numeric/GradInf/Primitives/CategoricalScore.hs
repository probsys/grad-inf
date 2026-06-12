module Numeric.GradInf.Primitives.CategoricalScore where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.Trans (lift)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.Categorical
import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Class definition

class (Monad m) => CategoricalScore m r tt where
    categoricalScore :: ([r], [tt]) -> m tt

-- Core semantics

instance CategoricalScore Sampler Double tt where
    categoricalScore = categorical

-- Supporting inference

instance
    (Categorical Sampler (Expected d d) tt) =>
    CategoricalScore Sampler (Expected d d) tt
    where
    categoricalScore = categorical

-- Support coupling + partial eval

instance
    ( MonadFactor d m
    , DeterministicPrimitives d i b mat
    , GetIndex tt i
    , Categorical Sampler d i
    , Mergeable t tt
    , Show d
    ) =>
    CategoricalScore (WrapperSamplerT m) (Coupled d) tt
    where
    categoricalScore (ps, vals) = do
        let psA = map (\(Coupled (pA, pB)) -> pA) ps
        let indices :: [i] = map fromInt [0 .. (length psA - 1)]
        iA <- primal (categorical (psA, indices))

        iB <-
            residual
                ( do
                    let psB = map (\(Coupled (pA, pB)) -> pB) ps
                    if any (\(pA, pB) -> pA == 0 && pB > 0) (zip psA psB)
                        then
                            error
                                ( "categoricalScore: only correct for pA > 0 when pB > 0 (psA = "
                                    ++ show psA
                                    ++ ", psB = "
                                    ++ show psB
                                    ++ ")"
                                )
                        else
                            return ()
                    score ((getIndex psB iA) / (getIndex psA iA))
                    return iA
                )

        return (merge (getIndex vals iA) (getIndex vals iB))
