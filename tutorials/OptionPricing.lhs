In this example, we use GradInf to develop a new gradient
estimator for a discrete stochastic model of option pricing.
The model simulates the price of an asset in accordance with the
[trinomial option pricing model](https://en.wikipedia.org/wiki/Trinomial_tree),
and returns the expected payoff of a
[European-style option](https://en.wikipedia.org/wiki/Option_style).

First, we define the model below.

\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RebindableSyntax #-}

import Prelude
import Control.Monad
import Control.Monad.Bayes.Sampler.Lazy
import Data.Functor.Identity
import Control.Applicative
import System.Random (setStdGen, mkStdGen)

import Numeric.GradInf
import Numeric.GradInf.Inference.Base
import Numeric.GradInf.Primitives.DeterministicPrimitives
import Numeric.GradInf.Primitives.CategoricalCRN
import Numeric.GradInf.Primitives.IterateP

-- $setup (use a fixed seed)
-- >>> setStdGen (mkStdGen 42)

optionPricingKernel :: forall m d i b mat.
    (DeterministicPrimitives d i b mat, CategoricalCRN m d d)
    => [d] -> d -> m d
optionPricingKernel theta x = do
    let r = theta !! 0
    let sigma = theta !! 1
    let deltaT = theta !! 2
    let denom = (exp (sigma * (sqrt (deltaT / 2)))
                 - exp (-sigma * (sqrt (deltaT / 2))))
    let sqrtPU = (exp (r * deltaT / 2)
                  - exp (-sigma * (sqrt (deltaT / 2))))
                 / denom
    let sqrtPD = (exp (sigma * (sqrt (deltaT / 2)))
                  - exp (r * deltaT / 2))
                 / denom
    let pU = sqrtPU * sqrtPU
    let pD = sqrtPD * sqrtPD
    let pM = 1 - pU - pD
    let u = exp (sigma * (sqrt (2 * deltaT)))
    let d = exp (-sigma * (sqrt (2 * deltaT)))
    x' <- categoricalCRN ([pU, pD, pM], [x * u, x * d, x])
    return x'

optionPricingModel :: forall m d i b mat.
    ( DeterministicPrimitives d i b mat
    , CategoricalCRN m d d, IterateP m d )
    => Int -> [d] -> m d
optionPricingModel n theta = do
    x <- iterateP (optionPricingKernel theta) 40 !! n
    let r = theta !! 0
    let deltaT = theta !! 2
    let t = fromIntegral n * deltaT
    return $ if (isGreater x 41) :: b then
        exp(-r * t) * (x - 41)
    else
        0
\end{code}

We now apply GradInf to compute gradient estimates.
For our inference algorithm, we will
use twisted Sequential Monte Carlo, supplying a simple twist
function that uses the perturbation in the *current* option
pricing value as an estimate of the perturbation in the *final*
option pricing value.

\begin{code}
twistFunc :: (Num d) => Int -> Coupled d -> d
twistFunc _ (Coupled (xL, xR)) = xR - xL

getGradientEstimates :: Sampler [Double]
getGradientEstimates = do
    let n = 50
    let deltaT = 0.5 / n
    replicateM 5 (fmap runIdentity (gradInfAD
      (optionPricingModel 50
        . (\r -> [r, 0.05, fromDouble deltaT])
        . runIdentity)
      (twistedSMCInferenceAlg twistFunc 1
        :: forall d i b. (Num d)
        => TwistedSMCInferenceAlg Coupling d i b)
      (Identity 0.15)))

-- |
-- >>> sampler getGradientEstimates
-- [16.859912080819587,13.889656771799789,23.11158408562197,6.798670922887533,10.00694667989766]
\end{code}

We can now calculate the empirical mean and variance of the
gradient estimator.
\begin{code}
meanAndVariance :: [Double] -> (Double, Double)
meanAndVariance xs = do
    let n = length xs
    let mu = (sum xs) / (fromIntegral n)
    let var = (sum (map (\x -> (x - mu) * (x - mu)) xs))
              / (fromIntegral (n - 1))
    (mu, var)

printMeanAndVariance :: IO ()
printMeanAndVariance = do
    samples <- sampler getGradientEstimates
    print (meanAndVariance samples)

-- |
-- >>> printMeanAndVariance
-- (14.133354108205307,39.73173399763917)
\end{code}
