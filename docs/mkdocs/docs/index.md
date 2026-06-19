<p align="center">
  <img src="images/logo.png#gh-light-mode-only" alt="GradInf Logo" height="60">
  <img src="images/logo-white.png#gh-dark-mode-only" alt="GradInf Logo" height="60">
</p>

# GradInf: Gradient Estimation as Probabilistic Inference

GradInf is a research package that provides the Haskell implementation
of *gradient inference*, a new approach to gradient
estimation described in
[this PLDI'26 paper](https://dl.acm.org/doi/abs/10.1145/3808321).

## Overview

<p align="center">
  <img src="images/workflow-core.png" alt="Core GradInf Workflow" width="800">
</p>

GradInf automatically synthesizes gradient estimators
(i.e., estimators of gradients of expected values [1]) for
probabilistic programs. Users define a probabilistic
program using a mix of ordinary Haskell and library-provided
primitives. They then call `gradInfAD`, specifying their desired
probabilistic inference strategy. The result is an unbiased estimate of
the gradient of the expectation
of the original program with respect to its parameter.

### Starter Example

We can write a simple queuing model:
```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

import Prelude hiding (flip)
import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.FlipCRN
import Numeric.GradInf.Primitives.IterateP

queueKernel :: forall m d i b mat.
    (DeterministicPrimitives d i b mat, FlipCRN m d b)
    => d -> i -> m i
queueKernel theta x = do
    let p = theta / (theta + if (isGreater x 25) :: b then 25 else toDouble x)
    b :: b <- flipCRN p
    let x' = if b then x + 1 else x - 1
    return x'

queueModel :: forall m d i b mat.
    (DeterministicPrimitives d i b mat, FlipCRN m d b, IterateP m i)
    => Int -> d -> m d
queueModel n theta = do
    x <- iterateP (queueKernel theta) 0 !! n
    return (toDouble x)
```
The annotated probability distributions (e.g. `flipCRN`) specify
*factorized coupling strategies*, which GradInf uses to form
lower variance gradient estimators.
We can now differentiate the program using the GradInf high-level API:
```haskell
import Data.Functor.Identity
import Numeric.GradInf

let thetaToDifferentiateAt = Identity 15.0
let n = 50
gradientEstimate <- sampler $ gradInfAD (queueModel n . runIdentity) (stratifiedImportanceResamplingInferenceAlg 1) thetaToDifferentiateAt
print gradientEstimate
```

Here, `stratifiedImportanceResamplingInferenceAlg 1` is an *inference strategy*
which enables sound and efficient gradient estimation.
See the tutorials in this documentation site for more details on the API.

  [1] *Monte Carlo Gradient Estimation in Machine Learning*, Shakir Mohamed, Mihaela Rosca, Michael Figurnov, and Andriy Mnih. *Journal of Machine Learning Research* 21, 132 (2020),

## Index

For more usage examples, check out the tutorial pages on applying GradInf to [queuing models](examples/QueueModel.md), [financial models](examples/OptionPricing.md), and [biochemical models](examples/GeneTranscription.md).
*API documentation and additional tutorials will be added soon!*

## Citation

```bibtex
@article{arya2026gradinf,
title     = {GradInf: Gradient Estimation as Probabilistic Inference},
author    = {Arya, Gaurav and Huot, Mathieu and Schauer, Moritz and Lew, Alexander K. and Saad, Feras A.},
journal   = {Proceedings of the ACM on Programming Languages},
volume    = {10},
number    = {PLDI},
articleno = {243},
month     = jun,
pages     = {1864--1890},
year      = {2026},
publisher = {Association for Computing Machinery},
address   = {New York, NY, USA},
doi       = {10.1145/3808321},
}
```
