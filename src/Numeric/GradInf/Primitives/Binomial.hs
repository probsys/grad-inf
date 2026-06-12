module Numeric.GradInf.Primitives.Binomial where

import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Random.Distribution.Binomial (integralBinomialPDF)

import Numeric.GradInf.Coupling
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.SMC
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.Primitives.Categorical
import Numeric.GradInf.Primitives.DeterministicPrimitives (
    DeterministicPrimitives,
 )

-- Class definition

class (Monad m) => Binomial m r i where
    binomial :: (i, r) -> m i

-- Core semantics

class BinomialGetP d i where
    binomialGetP :: (i, d) -> i -> d

instance BinomialGetP Double Int where
    binomialGetP (n, p) k = do
        let p0 = integralBinomialPDF n p k
        -- https://github.com/haskell-numerics/random-fu/issues/97
        if p == 0.0 || p == 1.0
            then
                log p0
            else
                p0

instance Binomial Sampler Double Int where
    binomial (n, p) = categorical ((map (binomialGetP (n, p)) [0 .. n]), [0 .. n])

-- Supporting inference

instance (BinomialGetP d i) => BinomialGetP (Expected d d) (Expected d i) where
    binomialGetP (Expected n, Expected p) (Expected k) = undefined

instance (Binomial m r i) => Binomial (StateT s m) r i where
    binomial = lift . binomial

instance Binomial Sampler (Expected d d) (Expected d i) where
    binomial (Expected n, Expected p) = error "binomial: undefined for Expected inputs"

instance (Binomial m d i) => Binomial (WeightedT d m) d i where
    binomial = lift . binomial

instance (Binomial m d i) => Binomial (PopulationT d m) d i where
    binomial = lift . binomial

-- Support coupling + partial eval

instance
    (Monad m0, DeterministicPrimitives d i b mat, Binomial Sampler d i) =>
    Binomial (WrapperSamplerT m0) (Coupled d) (Coupled i)
    where
    binomial (Coupled (nA, nB), Coupled (pA, pB)) = do
        if (nA /= nB) || (pA /= pB)
            then
                error ("binomial: only supports nA == nB and pA == pB")
            else
                return ()
        x <- primal (binomial (nA, pA))
        return (Coupled (x, x))
