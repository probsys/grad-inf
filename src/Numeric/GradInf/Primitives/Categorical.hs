module Numeric.GradInf.Primitives.Categorical where

import Control.Monad.Bayes.Class qualified as Bayes (categorical)
import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Vector qualified as BoxVector (fromList)

import Numeric.GradInf.Coupling
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.AD.AD
import Numeric.GradInf.AD.Backprop
import Numeric.GradInf.Inference.Base
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC.SMCTypes
import Numeric.GradInf.Inference.Sequential
import Numeric.GradInf.Primitives.DeterministicPrimitives


-- Class definition

class (Monad m) => Categorical m r tt where
    categorical :: ([r], [tt]) -> m tt

-- Core semantics

instance Categorical Sampler Double tt where
    categorical (ps, vals) = do
        if (abs (sum ps - 1)) > 1e-8
            then
                error
                    ( "categorical: Sampler semantics only supports normalized ps (sum ps = "
                        ++ (show (sum ps))
                        ++ ")"
                    )
            else do
                i <- Bayes.categorical (BoxVector.fromList ps)
                return (vals !! i)

-- Supporting inference

instance (Categorical Sampler d tt) => Categorical Sampler (Base d d) tt where
    categorical (psExpected, valsExpected) = categorical ((map (\(Base p) -> p) psExpected), valsExpected)

instance (Categorical m r tt) => Categorical (StateT s m) r tt where
    categorical = lift . categorical

instance (Categorical Sampler d tt) => Categorical Sampler (Expected d d) tt where
    categorical (psExpected, valsExpected) = categorical ((map (\(Expected p) -> p) psExpected), valsExpected)

instance (Categorical m d i) => Categorical (WeightedT d m) d i where
    categorical = lift . categorical

instance (Categorical m d i) => Categorical (PopulationT d m) d i where
    categorical = lift . categorical

instance
    (Num d) =>
    Categorical
        (Sequential susp)
        (Expected d d)
        [Coupled (Expected d t)]
    where
    categorical (psExpected, valsExpected) = do
        return
            ( foldl1
                ( \valExpected1 valExpected2 -> map (\(val1, val2) -> val1 + val2) (zip valExpected1 valExpected2)
                )
                ( map
                    ( \(Expected p, vals) ->
                        map
                            ( \(Coupled (Expected val1, Expected val2)) -> Coupled (Expected (p * val1), Expected (p * val2))
                            )
                            vals
                    )
                    (zip psExpected valsExpected)
                )
            )

instance Categorical Sampler (Forward Double) t where
    categorical (ps, vals) = do
        categorical (map extractForwardNum ps, vals)

instance (Categorical Sampler d t) => Categorical Sampler (AD s d) t where
    categorical (ps, vals) = categorical (map runAD ps, vals)

instance (Reifies s W) => Categorical Sampler (BVar s Double) t where
    categorical (ps, vals) = categorical (map extractBVar ps, vals)

-- Support coupling + partial eval

instance
    ( Monad m0
    , DeterministicPrimitives d i b mat
    , GetIndex tt i
    , Categorical Sampler d i
    ) =>
    Categorical (WrapperSamplerT m0) (Coupled d) tt
    where
    categorical (ps, vals) = do
        let psA = map (\(Coupled (pA, pB)) -> pA) ps
        let psB = map (\(Coupled (pA, pB)) -> pB) ps
        if psA /= psB
            then
                error ("categorical: only supports psA == psB")
            else
                return ()

        let indices :: [i] = map fromInt [0 .. (length psA - 1)]
        i <- primal (categorical (psA, indices))
        return (getIndex vals i)
