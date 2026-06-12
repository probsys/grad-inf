{-# LANGUAGE RebindableSyntax #-}

module Numeric.GradInf.Primitives.Flip where

import Control.Monad.Bayes.Class qualified as Bayes (bernoulli)
import Control.Monad.Bayes.Sampler.Lazy (Sampler)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Prelude hiding (flip)

import Numeric.GradInf.Coupling
import Numeric.GradInf.PartialProbabilityEval

import Numeric.GradInf.AD.AD
import Numeric.GradInf.AD.Backprop
import Numeric.GradInf.Inference.Base
import Numeric.GradInf.Inference.Expected
import Numeric.GradInf.Inference.Sequential
import Numeric.GradInf.Inference.SMC

import Numeric.GradInf.Primitives.DeterministicPrimitives

-- Class definition

class (Monad m) => Flip m r b where
    flip :: r -> m b

-- Core semantics

instance Flip Sampler Double Bool where
    flip p =
        if p < 0 || p > 1
            then
                error
                    ("flip: Sampler semantics only supports p in [0,1] (p = " ++ (show p) ++ ")")
            else
                Bayes.bernoulli p

-- Supporting inference

instance (Flip Sampler d b) => Flip Sampler (Base d d) (Base d b) where
    flip (Base p) = fmap Base (flip p)

instance (Flip m r b) => Flip (StateT s m) r b where
    flip = lift . flip

instance
    (Flip Sampler d b, BranchingConstructs b d, Num d) =>
    Flip Sampler (Expected d d) (Expected d b)
    where
    flip (Expected p) = fmap Expected (do b <- (flip p); return (if b :: b then 1 else 0))

instance Flip (Sequential susp) (Expected d d) (Expected d b) where
    flip (Expected p) = return (Expected p)

instance (Flip m d b) => Flip (WeightedT d m) d b where
    flip = lift . flip

instance (Flip m d b) => Flip (PopulationT d m) d b where
    flip = lift . flip

instance Flip Sampler (Forward Double) (Forward Bool) where
    flip = fmap pureForward . flip . extractForward

instance (Flip Sampler d b) => Flip Sampler (AD s d) (AD s b) where
    flip = fmap AD . flip . runAD

instance (Reifies s W) => Flip Sampler (BVar s Double) (BVar s Bool) where
    -- Intentionally used fmap instead of do notation because this gets the
    -- \*seeded* semantics to match the usual flip too, which is useful
    -- for fixed-seed testing.
    flip p = fmap (\seed -> mapBVar1 (seeded seed . flip) p) getSeed

-- Support coupling + partial eval

instance
    (Monad m0, DeterministicPrimitives d i b mat, Flip Sampler d b, Show d) =>
    Flip (WrapperSamplerT m0) (Coupled d) (Coupled b)
    where
    flip (Coupled (pA, pB)) = do
        if pA /= pB
            then
                error
                    ( "flip: only supports pA == pB (pA = " ++ show pA ++ ", pB = " ++ show pB ++ ")"
                    )
            else
                return ()
        x <- primal (flip pA)
        return (Coupled (x, x))
